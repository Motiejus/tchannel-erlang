-module(tchannel_conn_tests).

-include_lib("eunit/include/eunit.hrl").

internals_test_() ->
    [
     ?_assertEqual({ok, x1}, tchannel_conn:code_change(1, x1, []))
    ].
