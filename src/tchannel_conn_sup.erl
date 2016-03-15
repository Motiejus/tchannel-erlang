%%%-------------------------------------------------------------------
%% @doc supervisor of tchannel sockets.
%% @end
%%%-------------------------------------------------------------------

-module(tchannel_conn_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-define(CHILD(I, Type), {I, {I, start_link, []}, temporary, 5000, Type, [I]}).

-spec start_link() -> {ok, pid()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    {ok, {{simple_one_for_one, 1, 5}, [?CHILD(tchannel_conn, worker)]}}.
