-module(prop_tchannel_conn).
-ifdef(PROPER).

-include_lib("proper/include/proper.hrl").

small_packet() ->
    %union([N-2 || N <- lists:seq(16, 512)]).
    union([N-2 || N <- lists:seq(16, 32)]).

packet() ->
    ?LET(Bin,
         ?LET(Len, small_packet(), binary(Len)),
         <<(size(Bin)+2):16, Bin/binary>>).

few_packets() ->
    ?LET(N, union([1,2,3,5,7]), vector(N, packet())).

big_packet() ->
    ?LET(Pkts, few_packets(), {Pkts, iolist_size(Pkts), iolist_to_binary(Pkts)}).

big_packet_with_boundaries() ->
    ?LET({{Pkts, Size, BigPacket}, VectorSize},
         {big_packet(), union([0,1,2,3,4,5])},
         {Pkts, BigPacket, vector(VectorSize, integer(1, Size))}).

big_packet_split() ->
    ?LET({Pkts, BigPacket, Cutoff},
         big_packet_with_boundaries(),
         {Pkts, cutoff_bin(Cutoff, BigPacket)}).

cutoff_bin(Cutoff, BigPacket) ->
    Parts = cutoff([0|Cutoff] ++ [iolist_size(BigPacket)]),
    {<<>>, X} = lists:foldl(
      fun(Size, {Rem, Acc}) ->
              <<Part:Size/binary, Rest/binary>> = Rem,
              {Rest, [Part|Acc]}
      end,
      {BigPacket, []},
      Parts),
    lists:reverse(X).

%% @doc Converts breaking points in binary to sizes between the slices.
cutoff(Cutoff) ->
    cutoff(lists:usort(Cutoff), []).
cutoff([A], _) ->
    cutoff([0, A], []);
cutoff([A, B], Acc) ->
    lists:reverse([B - A | Acc]);
cutoff([A, B | Rest], Acc) ->
    cutoff([B|Rest], [B - A | Acc]).

%% Generate a N-length array with elements between 1 and PacketSize,
%% such that no two elements in the list are equal.
%%
%% OR coalesce the equal elements.

prop_boundaries() ->
    ?FORALL({Orig, Split},
            big_packet_split(),
            iolist_size(Orig) =:= iolist_size(Split)
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
