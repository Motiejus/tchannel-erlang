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

-type connect_option() ::
    {tcp_connect_timeout, timeout()} |
    {init_timeout, timeout()} |
    {tcp_options, [gen_tcp:connect_option()]}.

-type hostport() :: binary().
-type transport_header() ::
    {as, thrift | sthrift | json | http | raw} | % Arg Scheme, required
    {cas, hostport()}                          | % Claim At Start
    {caf, hostport()}                          | % Claim At Finish
    {cn, binary()}                             | % Caller Name, required
    {re, c | t | n | ct | tc}                  | % Retry Flags
    {se, '2'}                                  | % Speculative execution
    {fd, binary()}                             | % Failure Domain
    {sk, binary()}                             | % Shard Key
    {rd, binary()}.                              % Routing Delegate

-type msg_option() ::
    {headers, [transport_header()]}  | % required
    {ttl, pos_integer() | undefined} | % milliseconds. Optional.
    {tracing, binary()}.               % not supported

-type error_reason() ::
    {option, any()} | % bad argument
    connect_timeout | % timeout from gen_tcp:connect
    closed          | % remote end closed the connection
    inet:posix().

-type packet_id() :: 0..16#fffffffe.
-type packet_type_no() :: 0..16#ff.
