-module(tchannel_conn_tests).

-include_lib("eunit/include/eunit.hrl").

internals_test_() ->
    [
     ?_assertEqual({ok, x1}, tchannel_conn:code_change(1, x1, []))
    ].

-spec tcp_recv(Packets) -> {SplitPackets, Buffer, RemB} when
      Packets :: [binary()],
      SplitPackets :: [binary()],
      Buffer :: binary(),
      RemB :: non_neg_integer().
tcp_recv(Packets) ->
    lists:foldl(fun tchannel_conn:tcp_recv/2, {[], <<>>, undefined}, Packets).

%% @doc Just cover all branches of tcp_recv. PropEr test is also available.
tcp_recv_test_() ->
    [
     ?_assertEqual({[], <<0>>, undefined}, tcp_recv([<<0>>])),
     ?_assertEqual({[], <<0,3>>, 1}, tcp_recv([<<0,3>>])),
     ?_assertEqual({[], <<0,3>>, 1}, tcp_recv([<<0>>,<<3>>])),
     ?_assertEqual({[], <<0,4,0>>, 1}, tcp_recv([<<0,4>>,<<0>>])),
     ?_assertEqual({[<<0,3,0>>], <<0>>, undefined}, tcp_recv([<<0,3,0,0>>])),
     ?_assertEqual({[<<0,3,0>>], <<0,3>>, 1}, tcp_recv([<<0,3,0,0,3>>])),
     ?_assertEqual({[<<0,3,0>>], <<>>, undefined}, tcp_recv([<<0,3,0>>])),
     ?_assertEqual({[<<0,4,0,1>>], <<>>, undefined}, tcp_recv([<<0,4,0,1>>]))
    ].
