-module(tchannel_packet_tests).

-include_lib("eunit/include/eunit.hrl").

internals_test_() ->
    [
     ?_assertEqual(0, tchannel_packet:next_packet_id(16#fffffffe)),
     ?_assertEqual(1, tchannel_packet:next_packet_id(0)),
     ?_assertEqual(2, tchannel_packet:next_packet_id(1))
    ].
