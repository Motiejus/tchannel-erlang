-module(tchannel_conn_tests).

-include_lib("eunit/include/eunit.hrl").

internals_test_() ->
    [
     ?_assertEqual({ok, x1}, tchannel_conn:code_change(1, x1, [])),
     ?_assertEqual({ok, 1}, tchannel_conn:on_ok(ok, 1)),
     ?_assertEqual({error, 2}, tchannel_conn:on_ok({error, 2}, 1))
    ].
