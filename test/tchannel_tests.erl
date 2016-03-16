-module(tchannel_tests).

-include_lib("eunit/include/eunit.hrl").

tchannel_test_() ->
    {setup,
     fun() -> {ok, A} = application:ensure_all_started(tchannel), A end,
     fun(Apps) -> [application:stop(App) || App <- Apps] end,
     fun(Apps) ->
             [
              fun connect_timeout/0,
              fun connect_fail/0,
              fun init_req_tcp_fail/0
             ] ++
             [
              integration_(Apps)
             ]
     end
    }.


%% @doc Connect to 192.0.2.0/24 (RFC 5737). Should timeout.
connect_timeout() ->
    ?assertEqual({error, connect_timeout}, tchannel:connect("192.0.2.1", 2000)).

%% @doc Connect to 0.0.0.0:1. We assume nothing is listening...
connect_fail() ->
    ?assertEqual({error, econnrefused}, tchannel:connect("0.0.0.0", 1)).

init_req_tcp_fail() ->
    Self = self(),
    spawn(fun() -> gen_tcp_server(Self) end),
    Port = receive {port, Port1} -> Port1 end,
    ?assertEqual({error, closed}, tchannel:connect("127.0.0.1", Port)).

%% @doc Creates a TCP socket, waits for a connection, and closes the socket.
%%
%% Sends Caller {port, inet:port()}.
gen_tcp_server(Caller) ->
    {ok, LSock} = gen_tcp:listen(0, [{ip, {127,0,0,1}}]),
    {ok, Port} = inet:port(LSock),
    Caller ! {port, Port},
    {ok, Sock} = gen_tcp:accept(LSock),
    gen_tcp:close(Sock),
    gen_tcp:close(LSock).

%% @doc Integration test with tchannel_test.py
integration_(Apps) ->
    {setup,
     fun start_tchannel_echo/0,
     fun(_) -> ok end,
     fun({_Port, HostPort}) ->
             [
              ?_test(test_connect(HostPort))
             ]
     end
    }.

%% @doc Starts tchannel json echo service. Tells where it's listening on.
-spec start_tchannel_echo() -> {Port, HostPort} when
      Port :: port(),
      HostPort :: string().
start_tchannel_echo() ->
    {ok, Dir} = file:get_cwd(),
    Python = filename:join([Dir, "_build", "venv", "bin", "python"]),
    Server = filename:join([Dir, "test", "tchannel_echo.py"]),
    Port = open_port({spawn_executable, Python}, [{args, [Server]}]),
    HostPort = receive
        {Port, {data, Data}} ->
            string:strip(Data, both, $\n)
    end,
    {Port, HostPort}.

test_connect(HostPort) ->
    [Host, Port] = string:tokens(HostPort, ":"),
    {ok, T} = tchannel:connect(Host, list_to_integer(Port)),
    H = tchannel:headers(T),
    ?assertEqual(<<"python">>, proplists:get_value(<<"tchannel_language">>, H)),
    tchannel:close(T).
