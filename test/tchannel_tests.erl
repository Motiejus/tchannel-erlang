-module(tchannel_tests).

-include_lib("eunit/include/eunit.hrl").

%% @doc Starts tchannel json echo service. Tells where it's listening on.
-spec setup() -> {Port, HostPort} when
      Port :: port(),
      HostPort :: string().
setup() ->
    application:ensure_all_started(ringpoprop),
    {ok, Dir} = file:get_cwd(),
    Python = filename:join([Dir, "_build", "venv", "bin", "python"]),
    Server = filename:join([Dir, "test", "tchannel_echo.py"]),

    Port = open_port({spawn_executable, Python}, [{args, [Server]}]),
    HostPort = receive
        {Port, {data, Data}} ->
            string:strip(Data, both, $\n)
    end,
    {Port, HostPort}.

basic_exported_test_() ->
    {setup,
     fun setup/0,
     fun(_) -> ok end,  % echo server stops when stdin closes
     fun(Setup) ->
             [
              ?_test(server_started(Setup))
             ]
     end
    }.

server_started({_, HostPort}) ->
    [Host, Port] = string:tokens(HostPort, ":"),
    {ok, T} = tchannel:connect(Host, list_to_integer(Port)),
    ?assertEqual(<<"python">>, tchannel:header(T, <<"tchannel_language">>)),
    tchannel:close(T).
