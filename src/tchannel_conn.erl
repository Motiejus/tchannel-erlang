%%% @doc This gen_server holds the tcp socket, sends and receives data.
%%%
%%% The gen_server holds a list of subscribers attached. Every packet received
%%% through the socket needs to have a subscriber, otherwise it gets dropped.
%%% We might add support for a 'catch-all' subscriber later, but now it's not
%%% needed.

-module(tchannel_conn).

-behavior(gen_server).

-record(state, {
          socket :: gen_tcp:socket(),
          options :: [connect_option()],
          next_packet_id :: packet_id(),
          headers :: [{binary(), binary()}],
          version :: pos_integer(),  % tchannel version reported by remote
          registrees :: [{service(), pid()}],
          buffer :: binary(),  % incoming tcp buffer
          remb :: undefined | pos_integer() % remaining bytes for current packet
}).
-type state() :: #state{}.

-include("types.hrl").
-include("consts.hrl").

%% gen_server callbacks
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

%% API
-export([start_link/1]).

-ifdef(TEST).
-export([next_packet_id/1]).
-export([tcp_recv/2]).
-endif.

%%==============================================================================
%% gen_server API
%%==============================================================================

-spec start_link({Address, Port, Options}) ->
    {ok, Pid} | {stop, {error, Error}} when
      Address :: inet:ip_address() | inet:hostname(),
      Port :: inet:port_number(),
      Options :: [connect_option()],
      Pid :: pid(),
      Error :: error_reason().
start_link(Args) ->
    gen_server:start_link(?MODULE, [Args], []).


-spec init([{Address, Port, Options, Caller}]) ->
    {ok, State} | {stop, Reason} when
      Address :: inet:ip_address() | inet:hostname(),
      Port :: inet:port_number(),
      Options :: [connect_option()],
      Caller :: pid(),
      State :: state(),
      Reason :: error_reason().
init([{Address, Port, Options, Caller}]) ->
    init1(Address, Port, Options, Caller).


handle_call(headers, _From, State=#state{headers=Headers}) ->
    {reply, Headers, State};

handle_call({call_req, Service, Args, MsgOpts}, _From, State) ->
    {Ret, State1} = call_req(State, Service, Args, MsgOpts),
    {reply, Ret, State1};

handle_call(Request, From, State) ->
    lager:warning("Unknown call from ~p: ~p", [From, Request]),
    {reply, {error, invalid_request}, State}.


handle_cast(Request, State) ->
    lager:warning("Unknown cast: ~p", [Request]),
    {noreply, State}.


handle_info({tcp, _, Msg}, #state{buffer=Buf, remb=RemB, registrees=R}=State) ->
    {Packets, Buf1, RemainingBytes} = tcp_recv(Msg, {[], Buf, RemB}),
    [handle_full_packet(Packet, R) || Packet <- lists:reverse(Packets)],
    {noreply, State#state{buffer=Buf1, remb=RemainingBytes}};

%%% closed/errored tcp socket will just inform the registrees.
%handle_info({tcp_closed, S}, #state{registrees=Rs}=State) ->
%    lists:foreach(fun({_, R}) -> R ! {tchannel_closed, self()} end, Rs),
%    lager:debug("tcp '~p' closed, terminating '~p'", [S, self()]),
%    {stop, normal, State};
%handle_info({tcp_error, S, Reason}, #state{registrees=Rs}=State) ->
%    lists:foreach(fun({_, R}) -> R ! {tchannel_error, self(), Reason} end, Rs),
%    lager:debug("tcp '~p' error: '~p', terminating '~p'", [S, Reason, self()]),
%    {stop, normal, State};
handle_info(Info, State) ->
    lager:warning("Unknown info: ~p", [Info]),
    {noreply, State}.


terminate(Reason, #state{socket=Socket}) ->
    lager:debug("Terminating ~p because of ~p", [self(), Reason]),
    gen_tcp:close(Socket).


code_change(_Old, State, _Extra) ->
    {ok, State}.

%%==============================================================================
%% Internal functions
%%==============================================================================
init1(Address, Port, Options, Caller) ->
    process_flag(trap_exit, true),
    Timeout = proplists:get_value(tcp_connect_timeout, Options),
    TcpOpts = proplists:get_value(tcp_options, Options),
    Registrees = sofs:to_external(
                   sofs:product(
                     sofs:set(proplists:get_value(register, Options)),
                     sofs:set([Caller])
                    )
                  ),
    ConnectOpts = [binary, {active, false}, {nodelay, true}] ++ TcpOpts,
    case gen_tcp:connect(Address, Port, ConnectOpts, Timeout) of
        {ok, Socket} ->
            State = #state{socket=Socket,
                           options=Options,
                           registrees=Registrees},
            init_req(State);
        {error, timeout} ->
            {stop, connect_timeout};
        {error, Reason} ->
            {stop, Reason}
    end.

-spec init_req(State) -> {ok, State} | {stop, Reason} when
      State :: state(),
      Reason:: error_reason().
init_req(#state{socket=Socket}=State) ->
    Packet = construct_init_req(),
    case gen_tcp:send(Socket, Packet) of
        ok ->
            init_res(State);
        {error, Reason} ->
            {stop, Reason}
    end.

-spec init_res(State) -> {ok, State} | {stop, Reason} when
      State :: state(),
      Reason:: error_reason().
init_res(#state{socket=Socket, options=Options}=State) ->
    InitTimeout = proplists:get_value(init_timeout, Options),
    case recv_packet_passive(Socket, InitTimeout) of
        {ok, {init_res, _Id, Payload}} ->
            <<Version:16, NH:16, Rest/binary>> = Payload,
            Headers = parse_headers(Rest, NH),
            State2 = State#state{
                       version=Version,
                       headers=Headers,
                       next_packet_id=1},
            inet:setopts(Socket, [{active, once}]),
            {ok, State2};
        {error, Reason} ->
            {stop, Reason}
    end.

-spec call_req(State, Service, Args, MsgOpts) ->
    {ok, State} | {{error, Reason}, State} when
      State :: state(),
      Service :: binary(),
      Args :: {iodata(), iodata(), iodata()},
      MsgOpts :: [msg_option()],
      Reason :: inet:posix() | closed.
call_req(State, Service, {Arg1, Arg2, Arg3}, MsgOptions) ->
    PacketId = State#state.next_packet_id,
    Socket = State#state.socket,
    TTL = proplists:get_value(ttl, MsgOptions, ?DEFAULT_TTL),
    Headers = transport_headers(proplists:get_value(headers, MsgOptions)),
    Payload =
    [
     <<
       0:8,                               % XXX flags no fragmentation support
       TTL:32,                            % ttl
       0:200,                             % XXX tracing not supported
       (size(Service)):8, Service/binary  % service
     >>, Headers, <<                      % headers
       0:8                                % XXX csumtype not supported
     >>,
     <<(iolist_size(Arg1)):16>>, Arg1,
     <<(iolist_size(Arg2)):16>>, Arg2,
     <<(iolist_size(Arg3)):16>>, Arg3
    ],
    Packet = construct_packet(call_req, PacketId, Payload),
    State2 = State#state{next_packet_id = next_packet_id(PacketId)},
    {gen_tcp:send(Socket, Packet), State2}.

%% @doc Handle incoming data from the socket.
%%
%% First two bytes of the packet are size of the full packet, including the 2B
%% of size. All packet (including length) is stored in the buffer.  From the
%% protocol we know the smallest possible packet is 16B, so we can ignore
%% case length = 2.
%%
%% Tip for tchannel v3: subtract include size of the size packet. I.e.
%% PacketSizeInTChannelV3 := PacketSizeInTChannelV2 - 2.
-spec tcp_recv(Msg, {Packets, Buffer, Remaining}) ->
    {Packets, Buffer, Remaining} when
      Msg :: binary(),
      Packets :: [binary()],
      Buffer :: binary(),
      Remaining :: undefined | pos_integer().
%% Exit clause.
tcp_recv(<<>>, {Acc, Buffer, RemB}) ->
    {Acc, Buffer, RemB};

%% First byte only.
tcp_recv(<<FirstByte:8>>, {Acc, <<>>, undefined}) ->
    tcp_recv(<<>>, {Acc, <<FirstByte:8>>, undefined});
%% Have first byte, getting the second byte and possibly more.
tcp_recv(<<SecondByte:8, Rest/binary>>, {Acc, <<FirstByte:8>>, undefined}) ->
    <<PacketLen:16>> = <<FirstByte:8, SecondByte:8>>,
    tcp_recv(Rest, {Acc, <<PacketLen:16>>, PacketLen-2});

%% First 2B or more.
tcp_recv(<<PacketLength:16, Rest/binary>>, {Acc, _Buffer, undefined}) ->
    tcp_recv(Rest, {Acc, <<PacketLength:16>>, PacketLength-2});

%% Receiving not enough remaining bytes for the packet.
tcp_recv(Msg, {Acc, Buffer, RemB}) when size(Msg) < RemB ->
    tcp_recv(<<>>, {Acc, <<Buffer/binary, Msg/binary>>, RemB - size(Msg)});

%% Enough for the packet, might have a part of the next one.
tcp_recv(Msg, {Acc, Buf, RemB}) when size(Msg) >= RemB ->
    <<Remaining:RemB/binary, Rest/binary>> = Msg,
    Acc2 = [<<Buf/binary, Remaining/binary>>|Acc],
    tcp_recv(Rest, {Acc2, <<>>, undefined}).

handle_full_packet(_Packet, _Registrees) ->
    ok.

-spec parse_headers(Binary, NH) -> [{Key, Value}] when
      Binary :: binary(),
      NH :: pos_integer(),
      Key :: binary(),
      Value :: binary().
parse_headers(Binary, NH) ->
    {Headers, <<>>} = lists:mapfoldl(
                        fun(_, Rest1) ->
                                {Key, Rest2} = parse_header_item(Rest1),
                                {Val, Rest3} = parse_header_item(Rest2),
                                {{Key, Val}, Rest3}
                        end,
                        Binary,
                        lists:seq(1, NH)
                       ),
    Headers.

parse_header_item(<<Len:16, Rest/binary>>) ->
    <<Value:Len/binary, Rest1/binary>> = Rest,
    {Value, Rest1}.

-spec recv_packet_passive(Socket, Timeout) ->
    {ok, {Type, Id, Payload}} | {error, Reason} when
      Socket :: gen_tcp:socket(),
      Timeout :: timeout(),
      Id :: packet_id(),
      Type :: packet_type(),
      Payload :: binary(),
      Reason :: error_reason().
recv_packet_passive(Socket, Timeout) ->
    case gen_tcp:recv(Socket, 16, Timeout) of
        {ok, <<Size:16, TypeId:8, _Reserved1:8, Id:32, _Reserved2:64>>} ->
            case gen_tcp:recv(Socket, Size-16, Timeout) of
                {ok, Payload} ->
                    {ok, {type_name(TypeId), Id, Payload}};
                {error, Reason1} ->
                    {error, Reason1}
            end;
        {error, Reason2} ->
            {error, Reason2}
    end.


%% @doc Construct packet for init_req
-spec construct_init_req() -> Packet when Packet :: iodata().
construct_init_req() ->
    OTP = list_to_binary(erlang:system_info(otp_release)),
    Headers = [
               {<<"host_port">>, <<"0.0.0.0:0">>},
               {<<"process_name">>, <<"ringpoprop">>},
               {<<"tchannel_language">>, <<"erlang">>},
               {<<"tchannel_language_version">>, OTP},
               {<<"tchannel_version">>, ?TCHANNEL_LIB_VERSION}
              ],
    Payload =
    [
     <<?PROTOCOL_VERSION:16, (length(Headers)):16>>,
     <<
       <<(iolist_size(K)):16, K/binary,
         (iolist_size(V)):16, V/binary
       >> || {K, V} <- Headers
     >>
    ],
    construct_packet(init_req, 0, Payload).

%% @doc Construct transport headers from [transport_header()].
-spec transport_headers(Headers) -> Payload when
      Headers :: [transport_header()],
      Payload :: iodata().
transport_headers(Headers) ->
    [
     <<(length(Headers)):8>>,
     <<
       <<(iolist_size(bin(K))):8, (bin(K))/binary,
         (iolist_size(bin(V))):8, (bin(V))/binary
       >> || {K, V} <- Headers
     >>
    ].

%% @doc Construct a tchannel packet.
%%
%% Size must be <= 2^16, otherwise it will silently trim the size. Streaming
%% should be done at higher level.
-spec construct_packet(Type, Id, Payload) -> Packet when
      Type :: packet_type(),
      Id :: packet_id(),
      Payload :: iodata(),
      Packet :: iodata().
construct_packet(Type, Id, Payload) ->
    Size = iolist_size(Payload) + 16,
    TypeNum = type_num(Type),
    [<<Size:16,
      TypeNum:8,
      0:8,  % reserved byte
      Id:32,
      0:64>>, % 8 reserved bytes
     Payload].


%% @doc Numeric tchannel packet type.
-spec type_num(packet_type()) -> packet_type_no().
type_num(init_req) ->                16#01;
%type_num(init_res) ->               16#02;
type_num(call_req) ->                16#03.
%type_num(call_res) ->               16#04;
%type_num(call_req_continue) ->      16#13;
%type_num(call_res_continue) ->      16#14;
%type_num(cancel) ->                 16#c0;
%type_num(claim) ->                  16#c1;
%type_num(ping_req) ->               16#d0;
%type_num(ping_res) ->               16#d1;
%type_num(error) ->                  16#ff.

-spec type_name(packet_type_no()) -> packet_type().
%type_name(16#01) -> init_req;
type_name(16#02) -> init_res.
%type_name(16#03) -> call_req;
%type_name(16#04) -> call_res;
%type_name(16#13) -> call_req_continue;
%type_name(16#14) -> call_res_continue;
%type_name(16#c0) -> cancel;
%type_name(16#c1) -> claim;
%type_name(16#d0) -> ping_req;
%type_name(16#d1) -> ping_res;
%type_name(16#ff) -> error.

-spec bin(atom() | binary()) -> binary().
bin(X) when is_atom(X) -> atom_to_binary(X, utf8);
bin(X) when is_binary(X) -> X.

-spec next_packet_id(packet_id()) -> packet_id().
next_packet_id(16#fffffffe) -> 0;
next_packet_id(X) -> X + 1.
