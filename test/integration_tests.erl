-module(integration_tests).

-include_lib("eunit/include/eunit.hrl").

-define(conn(Opt), (tchannel:connect("127.0.0.1", 1, [Opt]))).

%% @doc Integration test with tchannel_test.py
integration_test_() ->
    {setup,
     fun setup/0,
     fun({Apps, _, _}) -> [application:stop(App) || App <- Apps] end,
     fun({_, _, HostPort}) ->
             [
              {"connect", ?_test(connect(HostPort))},
              {"connect with opts", ?_test(connect_with_opts(HostPort))},
              {"gen_server nocrash", ?_test(gen_server_api(HostPort))},
              {"call", ?_test(call(HostPort))},
              {"tchannel_closed", ?_test(closed(HostPort))}
             ]
     end
    }.

connect({Host, Port}) ->
    {ok, T} = tchannel:connect(Host, Port),
    H = tchannel:headers(T),
    ?assertEqual(<<"python">>, proplists:get_value(<<"tchannel_language">>, H)),
    tchannel:close(T).

%% @doc Connect with options
connect_with_opts({Host, Port}) ->
    Opts = [{tcp_connect_timeout, 500}, {init_timeout, 500}],
    {ok, T} = tchannel:connect(Host, Port, Opts),
    tchannel:close(T).

%% @doc Unknown messages don't crash the underlying gen_server
gen_server_api({Host, Port}) ->
    {ok, T} = tchannel:connect(Host, Port),
    ?assertEqual({error, invalid_request}, gen_server:call(T, invalid)),
    gen_server:cast(T, invalid),
    T ! invalid,
    % Check that server is responding ...
    ?assertMatch([_|_], tchannel:headers(T)),
    tchannel:close(T).

call({Host, Port}) ->
    {ok, T} = tchannel:connect(Host, Port),
    Opts = [{headers, [{as, json}, {cn, <<"tchannel-erlang-tests">>}]}],
    Args = {<<"/echo">>, <<>>, <<"1">>},
    {ok, Id} = tchannel:send(T, <<"echo-server">>, Args, Opts),
    receive
        {call_res, T, {Id, 0, _, _, {_, _, <<"1">>}}} ->
            ok
    end.

closed({Host, Port}) ->
    {ok, T} = tchannel:connect(Host, Port),
    Opts = [{headers, [{as, json}, {cn, <<"tchannel-erlang-tests">>}]}],
    Args = {<<"/exit">>, <<>>, <<"1">>},
    {ok, _} = tchannel:send(T, <<"echo-server">>, Args, Opts),
    receive
        {tchannel_closed, T} ->
            ok
    end.

%%==============================================================================
%% Utilities
%%==============================================================================

%% @doc Starts tchannel json echo service. Tells where it's listening on.
-spec setup() -> {Applications, Port, HostPort} when
      Applications :: [module()],
      Port :: port(),
      HostPort :: string().
setup() ->
    {ok, Apps} = application:ensure_all_started(tchannel),
    {ok, Dir} = file:get_cwd(),
    Python = filename:join([Dir, "_build", "venv", "bin", "python"]),
    Server = filename:join([Dir, "test", "tchannel_echo.py"]),
    Port = open_port({spawn_executable, Python}, [{args, [Server]}]),
    HostPort = receive
        {Port, {data, Data}} ->
            string:strip(Data, both, $\n)
    end,
    [Host, TcpPort] = string:tokens(HostPort, ":"),
    {Apps, Port, {Host, list_to_integer(TcpPort)}}.
