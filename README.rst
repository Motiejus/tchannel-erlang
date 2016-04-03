.. image:: https://api.travis-ci.org/Motiejus/tchannel-erlang.svg?branch=master
    :target: http://travis-ci.org/Motiejus/tchannel-erlang
.. image:: https://coveralls.io/repos/github/Motiejus/tchannel-erlang/badge.svg?branch=master
    :target: https://coveralls.io/github/Motiejus/tchannel-erlang?branch=master

TChannel-erlang
===============

TChannel driver for Erlang.

Build, compile and run tests
----------------------------

::

    $ make setup  ## requires `virtualenv` in `$PATH`.
    $ make

TODO
----

Things to do before any kind of announcement of this experiment:

* Documentation generator.

TChannel
========

This application implements a small enough subset of tchannel, just enough to
be able to establish a connection, keep it open, send and receive simple JSON
payloads.

Architecture
------------

Process diagram::

    1 tchannel_sup (app supervisor)
        1 tchannel_conn_sup (supervisor)
            m tchannel_conn (worker)

* ``tchannel_sup`` main application supervisor.
* ``tchannel_conn_sup`` supervisor of tchannel connections.
* ``tchannel_conn`` reads the incoming connection and messages the invoker.

One TChannel instance maps to one ``tchannel_conn`` worker (which holds the TCP
socket). The worker receives incoming messages from the socket, and forwards
them to the subscribed process. The reverse is also true.

This design makes the API is ``active``-only (similar to the ``active`` option
of ``gen_tcp:connect/3``), since the data of incoming socket must be processed
regardless of whether the caller is asking for it. For example, we need this to
be able to promptly respond to ``ping req``.

TChannel Client API
-------------------

See ``tchannel:option/0`` for more options::

  Opts = [{tcp_connect_timeout, 500},
          {init_timeout, 500}],
  {ok, Channel} = tchannel:connect("127.0.0.1", 3001, Opts),

Constructing headers for ``tchannel:send/3``. See `tchannel spec` for details::

  Headers = [{as, json}, {cn, <<"echoer">>}]

Contstructing outgoing message::

  MsgOpts = [{headers, Headers},    % required, see above
             {ttl, 1000}].          % not supported

Sending the actual message::

  Args = {Arg1, Arg2, Arg3},
  tchannel:send(TChannel, DestService, Args, MsgOpts),

Wait for the reply::

  receive
    {call_req, TChannel, {Id, TTL, Tracing, Service, Headers, Args}} ->
        ...;
    {call_res, TChannel, {Id, Code, Tracing, Headers, Args}} ->
        ...;
    {tchannel_closed, TChannel} ->
        ...;
    {tchannel_error, TChannel, Reason} ->
        ...
  end.

Contributing
------------

Unknown yet. Please contact me before making changes.

TODO
----

The API and implementation are minimalistic, because the intention is to
implement as less as possible without overthinking. Given you went thus far, we
lack:

1. The API for creating a channel, listening for it, terminating it is strange
   and incomplete. It will very likely be changed in the future.
2. Initial version will likely not implement >64K requests and responses.
3. In Erlang, a TChannel instance maps 1:1 to the underlying TCP connection. It
   is not true in Go/Node APIs, but is not mandated by the protocol. We'll know
   if we need to do that after actually using it for some time.
4. The creator of the channel "subscribes" to all the incoming messages
   automatically. There might be an intention to register per-service listeners.

.. _`tchannel spec`: http://tchannel.readthedocs.org/en/latest/protocol/
