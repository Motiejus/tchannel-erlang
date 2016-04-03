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
          caller :: pid(),           % receiver of all incoming traffic
          buffer :: binary(),        % incoming tcp buffer
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


handle_info({tcp, _, Msg}, #state{socket=Sock, buffer=Buf, remb=RemB}=State) ->
    {Pkts, Buf1, Remb1} = tchannel_packet:stream_recv(Msg, {[], Buf, RemB}),
    [handle_full_packet(Pkt, State#state.caller) || Pkt <- lists:reverse(Pkts)],
    inet:setopts(Sock, [{active, once}]),
    {noreply, State#state{buffer=Buf1, remb=Remb1}};

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
    ConnectOpts = [binary, {active, false}, {nodelay, true}] ++ TcpOpts,
    case gen_tcp:connect(Address, Port, ConnectOpts, Timeout) of
        {ok, Socket} ->
            State = #state{socket=Socket, options=Options, caller=Caller},
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
    Packet = tchannel_packet:construct_init_req(),
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
            {Version, Headers} = tchannel_packet:parse_init_res(Payload),
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
call_req(State, Service, Args, MsgOptions) ->
    PacketId = State#state.next_packet_id,
    Socket = State#state.socket,
    TTL = proplists:get_value(ttl, MsgOptions, ?DEFAULT_TTL),
    Headers = proplists:get_value(headers, MsgOptions),
    {Packet, PacketId2} = tchannel_packet:construct_call_req(
                            PacketId, TTL, Service, Headers, Args),
    State2 = State#state{next_packet_id = PacketId2},
    {gen_tcp:send(Socket, Packet), State2}.


handle_full_packet(_Packet, _Registrees) ->
    ok.


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
