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
              fun init_req_tcp_fail/0,
              fun first_send_fail/0,
              fun init_res_after_first_packet_fail/0,
              integration_(Apps)
             ]
     end
    }.


%% @doc Connect to 192.0.2.0/24 (RFC 5737). Should timeout.
connect_timeout() ->
    ?assertEqual({error, connect_timeout}, tchannel:connect("192.0.2.1", 1)).

%% @doc Connect to 0.0.0.0:1. We assume nothing is listening...
connect_fail() ->
    ?assertEqual({error, econnrefused}, tchannel:connect("0.0.0.0", 1)).

%% @doc Unable to receive init res from the remote.
init_req_tcp_fail() ->
    Port = start_server_get_port(fun gen_tcp_server_close/1),
    ?assertEqual({error, closed}, tchannel:connect("127.0.0.1", Port)).

%% @doc Connection establishment succeeds, but sending init_req payload fails.
first_send_fail() ->
    Port = start_server_get_port(fun gen_tcp_server_close/1),
    Opts = [{tcp_options, [{tcp_module, tchannel_inet_tcp_nosend}]}],
    ?assertEqual({error, closed}, tchannel:connect("127.0.0.1", Port, Opts)).

%% @doc Connection establishment succeeds, receive first header, connection err.
init_res_after_first_packet_fail() ->
    Port = start_server_get_port(fun gen_tcp_server_first_16b_only/1),
    ?assertEqual({error, closed}, tchannel:connect("127.0.0.1", Port)).

%% @doc Integration test with tchannel_test.py
integration_(_) ->
    {setup,
     fun start_tchannel_echo/0,
     fun({_Port, HostPort}) ->
             [
              ?_test(test_connect(HostPort))
             ]
     end
    }.

test_connect(HostPort) ->
    [Host, Port] = string:tokens(HostPort, ":"),
    {ok, T} = tchannel:connect(Host, list_to_integer(Port)),
    H = tchannel:headers(T),
    ?assertEqual(<<"python">>, proplists:get_value(<<"tchannel_language">>, H)),
    tchannel:close(T).

%%========================================================================================
%% Utilities
%%========================================================================================

start_server_get_port(Fun) ->
    Self = self(),
    spawn(fun() -> Fun(Self) end),
    receive
        {port, Port1} ->
            Port1
    end.

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

%% @doc Create a TCP socket, waits for a connection, and closes the socket.
%%
%% Sends Caller {port, inet:port()}.
gen_tcp_server_close(Caller) ->
    {ok, LSock} = gen_tcp:listen(0, [{ip, {127,0,0,1}}]),
    {ok, Port} = inet:port(LSock),
    Caller ! {port, Port},
    {ok, Sock} = gen_tcp:accept(LSock),
    gen_tcp:close(Sock),
    gen_tcp:close(LSock).

%% @doc Create a TCP socket, wait for a connection, send first 16b valid, close.
gen_tcp_server_first_16b_only(Caller) ->
    {ok, LSock} = gen_tcp:listen(0, [{ip, {127,0,0,1}}]),
    {ok, Port} = inet:port(LSock),
    Caller ! {port, Port},
    {ok, Sock} = gen_tcp:accept(LSock),
    % Claim size of the packet to be 32b. Send just header (16b).
    gen_tcp:send(Sock, <<32:16, 16#02:8, 0:8, 0:32, 0:64>>),
    gen_tcp:close(Sock),
    gen_tcp:close(LSock).

%% @doc Create a TCP socket, but delay accepting the connection.
%gen_tcp_server_noopen(Caller) ->
%    {ok, LSock} = gen_tcp:listen(0, [{ip, {127,0,0,1}}]),
%    {ok, Port} = inet:port(LSock),
%    Caller ! {port, Port},
%    receive
%        ok -> ok
%    end.
