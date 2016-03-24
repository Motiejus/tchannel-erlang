-module(prop_tchannel_conn).
-ifdef(PROPER).

-include_lib("proper/include/proper.hrl").

good_packet_size() ->
    union([N-2 || N <- [16, 1500, 1501, 9000, 9001, 65535]]).

packet() ->
    ?LET(Bin,
         ?LET(Len, good_packet_size(), binary(Len)),
         <<(size(Bin)+2):16, Bin/binary>>).

few_packets() ->
    ?LET(N, union([1,2,3,5,7]), vector(N, packet())).

big_packet() ->
    ?LET(Packets, few_packets(), iolist_to_binary(Packets)).

prop_packet_bounded() ->
    ?FORALL(
       Packet,
       big_packet(),
       begin
           size(Packet) < 7*16#ffffff
       end
      ).

prop_tcp_recv() ->
    ?FORALL(Packet,
            packet(),
            begin
                <<Size:16, Rest/binary>> = Packet,
                size(Rest) =:= Size - 2
            end
           ).

-endif.
