-module(tchannel_tests).

-include_lib("eunit/include/eunit.hrl").

-define(OPTS, [{host_port, <<"0.0.0.0:0">>}, {process_name, <<"woodootest">>}]).
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
              {"tcp connect timeout", fun connect_timeout/0},
              {"tcp connection failure", fun connect_fail/0},
              {"failure in 'init req'", fun init_req_tcp_fail/0},
              {"failure to send first packet", fun first_send_fail/0},
              {"'init res' fail after first packet",
               fun init_res_after_first_packet_fail/0}
             ]
     end
    }.

%% @doc Connection timeout, mocking inet_tcp module.
connect_timeout() ->
    Opts = ?OPTS ++ [{tcp_options, [{tcp_module, tchannel_inet_tcp_timeout}]}],
    Host = {192,0,2,1},
    ?assertEqual({error, connect_timeout}, tchannel:connect(Host, 1, Opts)).

%% @doc Connect to 127.0.0.1:1. Get eaccess...
connect_fail() ->
    % {error, eaccess} on osx, {error,enetunreach} on travis-ci/linux
    ?assertMatch({error, _}, tchannel:connect({255,255,255,255}, 1, ?OPTS)).

%% @doc Unable to receive init res from the remote.
init_req_tcp_fail() ->
    Port = start_server_get_port(fun gen_tcp_server_close/1),
    ?assertEqual({error, closed}, tchannel:connect({127,0,0,1}, Port, ?OPTS)).

%% @doc Connection establishment succeeds, but sending init_req payload fails.
first_send_fail() ->
    Port = start_server_get_port(fun gen_tcp_server_close/1),
    Opts = ?OPTS ++ [{tcp_options, [{tcp_module, tchannel_inet_tcp_nosend}]}],
    ?assertEqual({error, closed}, tchannel:connect({127,0,0,1}, Port, Opts)).

%% @doc Connection establishment succeeds, receive first header, connection err.
init_res_after_first_packet_fail() ->
    Port = start_server_get_port(fun gen_tcp_server_first_16b_only/1),
    ?assertEqual({error, closed}, tchannel:connect({127,0,0,1}, Port, ?OPTS)).

%%==============================================================================
%% Utilities
%%==============================================================================

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

start_server_get_port(Fun) ->
    Self = self(),
    spawn(fun() -> Fun(Self) end),
    receive
        {port, Port1} ->
            Port1
    end.

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
