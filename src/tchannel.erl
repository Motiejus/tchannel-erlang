-module(tchannel).

-include("types.hrl").
-export_type([option/0]).
-export_type([error_reason/0]).
-export_type([packet_id/0]).
-export_type([packet_type_no/0]).

-include("consts.hrl").

%% API
-export([connect/2]).
-export([connect/3]).
-export([headers/1]).
-export([header/2]).
-export([close/1]).

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
            Args = {Address, Port, merge_options(Options)},
            ChildSpecs = [{tchannel_conn, {tchannel_conn, start_link, [Args]},
                           temporary, 5000, worker, [tchannel_conn]}],
            supervisor:start_child(tchannel_conn_sup, ChildSpecs);
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

%%==============================================================================
%% Internal functions
%%==============================================================================
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

