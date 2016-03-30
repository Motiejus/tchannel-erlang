-module(prop_tchannel_conn).

-include_lib("proper/include/proper.hrl").


%% @doc Checks if TCP stream receiver can receive arbitrarily-cutoff packets.
%%
%% 1. Construct many valid length + payload packets. Merge them.
%% 2. Split the result to arbitrary chunks.
%% 3. Pass the list through tchannel_conn:tcp_recv/2 and verify they were
%%    properly reconstructed.
prop_tcp_recv() ->
    ?FORALL({Orig, Split},
            big_packet_split(),
            begin
                {Got, <<>>, undefined} = lists:foldl(
                  fun tchannel_conn:tcp_recv/2,
                  {[], <<>>, undefined},
                  Split),
                Orig =:= lists:reverse(Got)
            end
           ).

%%==============================================================================
%% Helpers
%%==============================================================================

%% @doc Generate a few valid packets, merge them, split them out arbitrarily.
big_packet_split() ->
    PacketSize = union([N-2 || N <- lists:seq(16, 512)]),
    PacketSizedBinary = ?LET(Len, PacketSize, binary(Len)),
    Packet = ?LET(Bin, PacketSizedBinary, <<(size(Bin)+2):16, Bin/binary>>),
    FewPackets = ?LET(N, union([1,2,3,5,7]), vector(N, Packet)),
    PCutoffs = ?LET({Pkts, VectorSize},
                    {FewPackets, union([0,1,2,3,4,5])},
                    {Pkts, vector(VectorSize, integer(1, iolist_size(Pkts)))}),
    ?LET({Pkts, Cutoff},
         PCutoffs,
         {Pkts, cutoff_bin(Cutoff, iolist_to_binary(Pkts))}).

%% @doc Given binary and cutoff array, do the actual cutoff.
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
cutoff([A, B], Acc) ->
    lists:reverse([B - A | Acc]);
cutoff([A, B | Rest], Acc) ->
    cutoff([B|Rest], [B - A | Acc]).
