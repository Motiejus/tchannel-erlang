%%% @doc inet_tcp-compatible module, returning {error, timeout} on connect/X.

-module(tchannel_inet_tcp_timeout).

-export([getaddrs/2, getserv/1, connect/4]).

getaddrs({192,0,2,1}, _Timer) -> {ok,[{192,0,2,1}]}.

getserv(Port) when is_integer(Port) -> {ok, Port}.

connect(_Address, _Port, _Opts, _Timeout) ->
    {error, timeout}.
