-module(tchannel_tests).

-include_lib("eunit/include/eunit.hrl").

-define(conn(Opt), (tchannel:connect("127.0.0.1", 1, [Opt]))).

api_test_() ->
    [
     ?_assertEqual(
        {error, {option, {tcp_connect_timeout, x}}},
        ?conn({tcp_connect_timeout, x})
       ),
     ?_assertEqual(
        {error, {option, {init_timeout, x}}},
        ?conn({init_timeout, x})
       ),
     ?_assertEqual(
        {error, {option, {tcp_options, x}}},
        ?conn({tcp_options, x})
       )
    ].

tchannel_test_() ->
    {setup,
     fun() -> {ok, A} = application:ensure_all_started(tchannel), A end,
     fun(Apps) -> [application:stop(App) || App <- Apps] end,
     fun(_) ->
             [
              ?_assertEqual({ok, x1}, tchannel_conn:code_change(1, x1, [])),
              {"tcp connect timeout", fun connect_timeout/0},
              {"tcp connection failure", fun connect_fail/0},
              {"failure in 'init req'", fun init_req_tcp_fail/0},
              {"failure to send first packet", fun first_send_fail/0},
              {"'init res' fail after first packet",
               fun init_res_after_first_packet_fail/0},
              integration_()
             ]
     end
    }.


%% @doc Connection timeout, mocking inet_tcp module.
connect_timeout() ->
    Opts = [{tcp_options, [{tcp_module, tchannel_inet_tcp_timeout}]}],
    Host = {192,0,2,1},
    ?assertEqual({error, connect_timeout}, tchannel:connect(Host, 1, Opts)).

%% @doc Connect to 127.0.0.1:1. Get eaccess...
connect_fail() ->
    % {error, eaccess} on osx, {error,enetunreach} on travis-ci/linux
    ?assertMatch({error, _}, tchannel:connect({255,255,255,255}, 1)).

%% @doc Unable to receive init res from the remote.
init_req_tcp_fail() ->
    Port = start_server_get_port(fun gen_tcp_server_close/1),
    ?assertEqual({error, closed}, tchannel:connect({127,0,0,1}, Port)).

%% @doc Connection establishment succeeds, but sending init_req payload fails.
first_send_fail() ->
    Port = start_server_get_port(fun gen_tcp_server_close/1),
    Opts = [{tcp_options, [{tcp_module, tchannel_inet_tcp_nosend}]}],
    ?assertEqual({error, closed}, tchannel:connect({127,0,0,1}, Port, Opts)).

%% @doc Connection establishment succeeds, receive first header, connection err.
init_res_after_first_packet_fail() ->
    Port = start_server_get_port(fun gen_tcp_server_first_16b_only/1),
    ?assertEqual({error, closed}, tchannel:connect({127,0,0,1}, Port)).

%% @doc Integration test with tchannel_test.py
integration_() ->
    {setup,
     fun start_tchannel_echo/0,
     fun({_Port, {Host, Port}}) ->
             [
              {"test connect", ?_test(connect({Host, Port}))},
              {"test connect with opts", ?_test(connect_with_opts({Host, Port}))},
              {"gen_server nocrash", ?_test(gen_server_api({Host, Port}))},
              {"test call", ?_test(call({Host, Port}))}
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
    Sub = tchannel:create_sub(T, <<"echo-server">>),
    Headers = [{as, json}, {cn, tchannel_unit}, {fd, <<"echo-services">>}],
    ok = tchannel:send(Sub, <<"/echo">>, <<>>, <<"1">>, [{headers, Headers}]).

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
    [Host, TcpPort] = string:tokens(HostPort, ":"),
    {Port, {Host, list_to_integer(TcpPort)}}.

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
