.. image:: https://api.travis-ci.org/Motiejus/tchannel-erlang.png?branch=master
    :target: http://travis-ci.org/Motiejus/tchannel-erlang

TChannel-erlang
===============

TChannel driver for Erlang.

Build, compile and run tests
----------------------------

::

    $ make setup  ## requires `virtualenv` in `$PATH`).
    $ make

TODO
----

Things to do before any kind of announcement of this experiment:

* Documentation generator.
* Coveralls.

TChannel
========

This application implements a small enough subset of tchannel, just enough to
be able to establish a connection, keep it open, send and receive simple
(<64KB) JSON payloads.

API
---

The sketch of API is as follows.

See tchannel:option/0 for more options::

  Opts = [{tcp_connect_timeout, 500},
          {init_timeout, 500}],

Establishes the TCP connection and initializes tchannel state::

  {ok, Channel} = tchannel:connect("127.0.0.1:3001", <<"sender">>, Options),

Constructing headers for ``tchannel:send/3``. See tchannel spec for details::

  Headers = [{as, <<"json">>},   % required
             {cas, undefined},   % optional
             {cn, <<"echoer">>}, % required
             {re, undefined},    % optional
             {se, undefined},    % optional
             {fd, undefined},    % optional
             {sk, undefined}     % optional
             {rd, undefined}],   % optional

Getting a sub-channel and subscribing to the messages (I acknowledge the API
isn't very idiomatic, and will likely change here)::

  {ok, SubChannel} = tchannel:create_sub(Channel, <<"destination_service">>),

Contstructing outgoing message::

  MsgOpts = [{packet_id, 1234},     % required
             {headers, Headers},    % required, see above
             {ttl, 1000},           % optional
             {tracing, undefined}], % not supported

Sending the actual message::

  tchannel:send(SubChannel, Arg1, Arg2, Arg3, MsgOpts),

Wait for the reply::

  receive
    {tchannel, SubChannel, {Id, Code, Tracing, Headers, Arg1, Arg2, Arg3}} ->
        react_data(Arg1, Arg2, Arg3);
    {tchannel_closed, SubChannel} ->
        react_closed(SubChannel)
  end.

Architecture
------------

I expect to make this true in short-term::

    1 tchannel_sup (app supervisor)
        1 tchannel_conn_sup (supervisor)
            m tchannel_conn (worker)

* ``tchannel_sup`` main application supervisor.
* ``tchannel_conn_sup`` supervisor of tchannel connections.
* ``tchannel_conn`` reads the incoming connection and hands over stuff to
  listeners.

Contributing
------------

Unknown yet. Please contact me before making patches.

TODO
----

The API and implementation are explicitly minimalistic, because the intention
is to implement as less as possible without overthinking. It is better to make
big changes when the codebase is small and limited, rather than big and
complete. With a good introduction, we lack:

1. The API for creating a channel, listening for it, terminating it is strange.
   It will very likely be changed in the future.
2. Initial version will definitely not implement >64K requests and responses.
3. In Erlang, a TChannel instance maps 1:1 to the underlying TCP connection. It
   is not true in Go/Node APIs, but is not mandated by the protocol. We'll know
   if we need to do that after actually using it for some time.
