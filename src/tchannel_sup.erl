%%%-------------------------------------------------------------------
%% @doc tchannel top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(tchannel_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    Procs = [{tchannel_conn_sup, {tchannel_conn_sup, start_link, []},
              permanent, 5000, supervisor, [tchannel_conn_sup]}],
    {ok, {{one_for_one, 1, 5}, Procs}}.

%%====================================================================
%% Internal functions
%%====================================================================
