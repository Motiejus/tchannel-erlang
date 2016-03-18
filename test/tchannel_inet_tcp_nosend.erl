%%% @doc inet_tcp-compatible module, returning {error, closed} on send/2.

-module(tchannel_inet_tcp_nosend).

-export([getaddrs/2, getserv/1, connect/4, send/2]).

getaddrs({127,0,0,1}, _Timer) -> {ok,[{127,0,0,1}]}.

getserv(Port) when is_integer(Port) -> {ok, Port}.

send(_Socket, _Packet) ->
    {error, closed}.

%%==============================================================================
%% copied from kernel/src/inet_int.hrl
%%==============================================================================
-define(ip(A,B,C,D),
        (((A) bor (B) bor (C) bor (D)) band (bnot 16#ff)) =:= 0).
-define(port(P), (((P) band bnot 16#ffff) =:= 0)).
-record(connect_opts, 
        { 
          ifaddr = any :: any(),  %% bind to interface address
          port   = 0   :: any(),  %% bind to port (default is dynamic port)
          fd     = -1  :: any(),  %% fd >= 0 => already bound
          opts   = []  :: any()   %% [{active,true}] added in inet:connect_options
         }).

%%==============================================================================
%% Copied from inet_tcp.erl
%%==============================================================================
connect(Address, Port, Opts, Timeout) when is_integer(Timeout), 
                                           Timeout >= 0 ->
    do_connect(Address, Port, Opts, Timeout).

do_connect({A,B,C,D}, Port, Opts, Time) when ?ip(A,B,C,D), ?port(Port) ->
    case inet:connect_options(Opts, inet) of
        %{error, Reason} -> exit(Reason);
        {ok, #connect_opts{fd=Fd,
                           ifaddr=BAddr={Ab,Bb,Cb,Db},
                           port=BPort,
                           opts=SockOpts}}
        when ?ip(Ab,Bb,Cb,Db), ?port(BPort) ->
            case inet:open(Fd,BAddr,BPort,SockOpts,tcp,inet,stream,?MODULE) of
                {ok, S} ->
                    case prim_inet:connect(S, {A,B,C,D}, Port, Time) of
                        ok    -> {ok,S}
                        %Error ->  prim_inet:close(S), Error
                    end
                %Error -> Error
            end
        %{ok, _} -> exit(badarg)
    end.
