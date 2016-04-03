-module(tchannel_packet_tests).

-include_lib("eunit/include/eunit.hrl").

internals_test_() ->
    [
     ?_assertEqual(0, tchannel_packet:next_packet_id(16#fffffffe)),
     ?_assertEqual(1, tchannel_packet:next_packet_id(0)),
     ?_assertEqual(2, tchannel_packet:next_packet_id(1))
    ].

-spec stream_recv(Packets) -> {SplitPackets, Buffer, RemB} when
      Packets :: [binary()],
      SplitPackets :: [binary()],
      Buffer :: binary(),
      RemB :: non_neg_integer().
stream_recv(Packets) ->
    lists:foldl(
      fun tchannel_packet:stream_recv/2,
      {[], <<>>, undefined},
      Packets
     ).

%% @doc Just cover all branches of stream_recv. PropEr test is also available.
stream_recv_test_() ->
    [
     ?_assertEqual({[], <<0>>, undefined}, stream_recv([<<0>>])),
     ?_assertEqual({[], <<0,3>>, 1}, stream_recv([<<0,3>>])),
     ?_assertEqual({[], <<0,3>>, 1}, stream_recv([<<0>>,<<3>>])),
     ?_assertEqual({[], <<0,4,0>>, 1}, stream_recv([<<0,4>>,<<0>>])),
     ?_assertEqual({[<<0,3,0>>], <<0>>, undefined}, stream_recv([<<0,3,0,0>>])),
     ?_assertEqual({[<<0,3,0>>], <<0,3>>, 1}, stream_recv([<<0,3,0,0,3>>])),
     ?_assertEqual({[<<0,3,0>>], <<>>, undefined}, stream_recv([<<0,3,0>>])),
     ?_assertEqual({[<<0,4,0,1>>], <<>>, undefined}, stream_recv([<<0,4,0,1>>]))
    ].
