
-type packet_type() ::
    init_req          | % First message on every connection must be init
    init_res          | % Remote response to init req
    call_req          | % RPC method request
    call_res          | % RPC method response
    call_req_continue | % RPC request continuation fragment
    call_res_continue | % RPC response continuation fragment
    cancel            | % Cancel an outstanding call req / forward req (no body)
    claim             | % Claim / cancel a redundant request
    ping_req          | % Protocol level ping req (no body)
    ping_res          | % Ping res (no body)
    error.              % Protocol level error

-type option() ::
    {tcp_connect_timeout, timeout()} |
    {init_timeout, timeout()}.

-type error_reason() ::
    {option, any()} |
    connect_timeout | % timeout from gen_tcp:connect
    closed |
    protocol |  % received something we don't expect
    inet:posix().

-type packet_id() :: 0..16#fffffffe.
-type packet_type_no() :: 0..16#ff.

