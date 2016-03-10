-module(tchannel_conn_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-spec start_link() -> {ok, pid()}.
start_link() ->
	supervisor:start_link(?MODULE, []).

init([]) ->
    {ok, {{one_for_one, 1, 5}, []}}.
