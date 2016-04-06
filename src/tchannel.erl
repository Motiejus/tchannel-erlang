-module(tchannel).

-include("consts.hrl").
-include("types.hrl").

%% Opaques of this module
-opaque tchannel() :: pid().
-export_type([tchannel/0]).

%% Public data types
-export_type([connect_option/0]).
-export_type([error_reason/0]).
-export_type([packet_id/0]).
-export_type([packet_type_no/0]).

%% Public API
-export([close/1]).
-export([connect/2]).
-export([connect/3]).
-export([headers/1]).
-export([send/4]).

%% @doc Connect to a tchannel endpoint with default options.
-spec connect(Address, Port) -> {ok, Channel} | {error, Reason} when
      Address :: inet:ip_address() | inet:hostname(),
      Port :: inet:port_number(),
      Channel :: tchannel(),
      Reason :: error_reason().
connect(Address, Port) ->
    connect(Address, Port, []).

%% @doc Connect to a tchannel endpoint.
%%
%% There are two kinds of timeouts in the Options:
%% * tcp_connect_timeout :: time limit to establish TCP session.
%% * init_timeout :: time limit to do a phase per init. Actual init time
%%   will be a few of times longer.
%% TODO: respect this value.
-spec connect(Address, Port, Options) -> {ok, Channel} | {error, Reason} when
      Address :: inet:ip_address() | inet:hostname(),
      Port :: inet:port_number(),
      Options :: [connect_option()],
      Channel :: tchannel(),
      Reason :: error_reason().
connect(Address, Port, Options) ->
    connect1(Address, Port, Options).

-spec send(TChannel, Service, Args, MsgOpts) ->
    {ok, Id} | {error, Error} when
      TChannel :: tchannel(),
      Service :: service(),
      Args :: {iodata(), iodata(), iodata()},
      MsgOpts :: [msg_option()],
      Id :: packet_id(),
      Error :: inet:posix() | closed.
send(TChannel, Service, Args, MsgOpts) ->
    gen_server:call(TChannel, {call_req, Service, Args, MsgOpts}).

%% @doc List of headers returned by remote party on 'init res'.
-spec headers(TChannel) -> [{HeaderKey, HeaderVal}] when
      TChannel :: tchannel(),
      HeaderKey :: binary(),
      HeaderVal :: binary().
headers(TChannel) ->
    gen_server:call(TChannel, headers).

%% @doc Close the TChannel by killing the underlying child.
%%
%% Will terminate the TCP socket.
-spec close(TChannel) -> ok when
      TChannel :: tchannel().
close(TChannel) ->
    ok = supervisor:terminate_child(tchannel_conn_sup, TChannel).

%%==============================================================================
%% Internal functions
%%==============================================================================
connect1(Address, Port, Options) ->
    case check_options(Options) of
        ok ->
            Args = {Address, Port, merge_options(Options), self()},
            supervisor:start_child(tchannel_conn_sup, [Args]);
        Error ->
            Error
    end.

check_options([]) ->
    ok;
check_options([{tcp_connect_timeout, T}|Options]) when is_integer(T) ->
    check_options(Options);
check_options([{init_timeout, T}|Options]) when is_integer(T) ->
    check_options(Options);
check_options([{tcp_options, L}|Options]) when is_list(L) ->
    check_options(Options);
check_options([Opt|_]) ->
    {error, {option, Opt}}.

%% @doc Merges user-defined and default options
-spec merge_options([connect_option()]) -> [connect_option()].
merge_options(Options) ->
    F = fun(K, Default) -> {K, proplists:get_value(K, Options, Default)} end,
    [
     F(tcp_connect_timeout, ?DEFAULT_TCP_CONNECT_TIMEOUT),
     F(init_timeout, ?DEFAULT_INIT_TIMEOUT),
     F(tcp_options, [])
    ].
