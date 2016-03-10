-module(tchannel).

-define(TCHANNEL_LIB_VERSION, <<"0.1">>).  % version of this tchannel library
-define(PROTOCOL_VERSION, 2).

-define(DEFAULT_TCP_CONNECT_TIMEOUT, 500).
-define(DEFAULT_INIT_TIMEOUT, 500).

-type packet_type() ::
    init_req          | % First message on every connection must be init
    init_res          | % Remote response to init req
    call_req          | % RPC method request
    call_res          | % RPC method response
    call_req_continue | % RPC request continuation fragment
    call_res_continue | % RPC response continuation fragment
    cancel            | % Cancel an outstanding call req / forward req (no body)
    claim             | % Claim / cancel a redundant request
    ping_req          | % Protocol level ping req (no body)
    ping_res          | % Ping res (no body)
    error.              % Protocol level error

-type option() ::
    {tcp_connect_timeout, timeout()} |
    {init_timeout, timeout()}.

-type error_reason() ::
    {option, any()} |
    connect_timeout | % timeout from gen_tcp:connect
    closed |
    protocol |  % received something we don't expect
    inet:posix().

-type packet_id() :: 0..16#fffffffe.
-type packet_type_no() :: 0..16#ff.

-record(state, {
          sock :: gen_tcp:socket(),
          options :: [option()],
          headers :: [{binary(), binary()}],
          version :: pos_integer()  % tchannel version reported by remote
}).
-opaque state() :: #state{}.
-export_type([state/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export([connect/2, connect/3, headers/1, header/2, close/1]).

connect(Address, Port) ->
    connect(Address, Port, []).

%% @doc Connect to a tchannel endpoint.
%%
%% There are two kinds of time outs in the Options:
%% * tcp_connect_timeout :: time limit to establish TCP session.
%% * init_timeout :: time limit to do a phase per init. Actual init time
%%   will be a few of times longer. TODO: use this value properly.
-spec connect(Address, Port, Options) -> {ok, State} | {error, Reason} when
      Address :: inet:ip_address() | inet:hostname(),
      Port :: inet:port_number(),
      Options :: [option()],
      State :: state(),
      Reason :: error_reason().
connect(Address, Port, Options) ->
    case check_options(Options) of
        ok ->
            connect_1(Address, Port, merge_options(Options));
        Error ->
            Error
    end.

-spec headers(State) -> [{HeaderKey, HeaderVal}] when
      State :: state(),
      HeaderKey :: binary(),
      HeaderVal :: binary().
headers(#state{headers=Headers}) ->
    Headers.

-spec header(State, HeaderKey) -> HeaderVal when
      State :: state(),
      HeaderKey :: binary(),
      HeaderVal :: undefined | binary().
header(#state{headers=Headers}, HeaderKey) ->
    proplists:get_value(HeaderKey, Headers).

-spec close(State) -> ok when
      State :: state().
close(#state{sock=Socket}) ->
    gen_tcp:close(Socket).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal stuff
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

check_options([]) ->
    ok;
check_options([{tcp_connect_timeout, T}|Options]) when is_integer(T) ->
    check_options(Options);
check_options([{init_timeout, T}|Options]) when is_integer(T) ->
    check_options(Options);
check_options([Opt|_]) ->
    {error, {options, Opt}}.

%% @doc Merges user-defined and default options
-spec merge_options([option()]) -> [option()].
merge_options(Options) ->
    F = fun(K, Default) -> {K, proplists:get_value(K, Options, Default)} end,
    [
     F(tcp_connect_timeout, ?DEFAULT_TCP_CONNECT_TIMEOUT),
     F(init_timeout, ?DEFAULT_INIT_TIMEOUT)
    ].

connect_1(Address, Port, Options) ->
    Timeout = proplists:get_value(tcp_connect_timeout, Options),
    case gen_tcp:connect(Address, Port, [binary, {active, false}], Timeout) of
        {ok, Sock} ->
            State = #state{sock=Sock, options=Options},
            init_req(State);
        {error, timeout} ->
            {error, connect_timeout};
        {error, Reason} ->
            {error, Reason}
    end.

-spec init_req(State) -> {ok, State} | {error, Reason} when
      State :: state(),
      Reason:: error_reason().
init_req(#state{sock=Sock}=State) ->
    Packet = construct_init_req(),
    case gen_tcp:send(Sock, Packet) of
        ok ->
            init_res(State);
        {error, Reason} ->
            {error, Reason}
    end.

-spec init_res(State) -> {ok, State} | {error, Reason} when
      State :: state(),
      Reason:: error_reason().
init_res(#state{sock=Sock, options=Options}=State) ->
    case recv_packet(Sock, proplists:get_value(init_timeout, Options)) of
        {ok, {init_res, Id, Payload}} ->
            init_res_1(State, Id, Payload);
        {error, Reason} ->
            {error, Reason}
    end.

init_res_1(State, _Id, Payload) ->
    <<Version:16, NH:16, Rest/binary>> = Payload,
    %lager:info("size(Rest): ~p, Rest: ~p", [size(Rest), Rest]),
    Headers = parse_headers(Rest, NH),
    State2 = State#state{version=Version, headers=Headers},
    {ok, State2}.

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

-spec recv_packet(Sock, Timeout) ->
    {ok, {Type, Id, Payload}} | {error, Reason} when
      Sock :: gen_tcp:socket(),
      Timeout :: timeout(),
      Id :: packet_id(),
      Type :: packet_type(),
      Payload :: binary(),
      Reason :: error_reason().
recv_packet(Sock, Timeout) ->
    case gen_tcp:recv(Sock, 16, Timeout) of
        {ok, <<Size:16, TypeId:8, _Reserved1:8, Id:32, _Reserved2:64>>} ->
            case gen_tcp:recv(Sock, Size-16, Timeout) of
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
    HeaderPayload =
    [
     <<(length(Headers)):16>>,
     [<<
        (iolist_size(K)):16, K/binary,
        (iolist_size(V)):16, V/binary
      >> || {K, V} <- Headers
     ]
    ],

    Payload = [<<?PROTOCOL_VERSION:16>>, HeaderPayload],
    construct_packet(init_req, 1, Payload).


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
type_num(init_req) ->                16#01.
%type_num(init_res) ->               16#02;
%type_num(call_req) ->               16#03;
%type_num(call_res) ->               16#04;
%type_num(call_req_continue) ->      16#13;
%type_num(call_res_continue) ->      16#14;
%type_num(cancel) ->                 16#c0;
%type_num(claim) ->                  16#c1;
%type_num(ping_req) ->               16#d0;
%type_num(ping_res) ->               16#d1;
%type_num(error) ->                  16#ff.

-spec type_name(packet_type_no()) -> packet_type().
type_name(16#01) -> init_req;
type_name(16#02) -> init_res;
type_name(16#03) -> call_req;
type_name(16#04) -> call_res;
type_name(16#13) -> call_req_continue;
type_name(16#14) -> call_res_continue;
type_name(16#c0) -> cancel;
type_name(16#c1) -> claim;
type_name(16#d0) -> ping_req;
type_name(16#d1) -> ping_res;
type_name(16#ff) -> error.
