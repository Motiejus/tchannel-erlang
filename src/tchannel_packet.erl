%%% @doc Pure packet creator and parser for tchannel_conn.
%%%
-module(tchannel_packet).

%%% Non-pure
-export([recv_packet_passive/2]).

%% Pure
-export([construct_init_req/2,
         parse_init_res/1,
         parse_call_res/1,
         parse_full_packet/1,
         stream_recv/2,
         construct_call_req/5]).

-ifdef(TEST).
-export([next_packet_id/1]).
-endif.

-include("types.hrl").
-include("consts.hrl").

%%=============================================================================
%% Non-pure functions
%%=============================================================================

%%% @doc Receive a tchannel packet in passive mode. Non-pure.
-spec recv_packet_passive(Socket, Timeout) ->
    {ok, {Type, Id, Payload}} | {error, Reason} when
      Socket :: gen_tcp:socket(),
      Timeout :: timeout(),
      Id :: packet_id(),
      Type :: packet_type(),
      Payload :: binary(),
      Reason :: error_reason().
recv_packet_passive(Socket, Timeout) ->
    case gen_tcp:recv(Socket, 16, Timeout) of
        {ok, <<Size:16, TypeId:8, _Reserved1:8, Id:32, _Reserved2:64>>} ->
            case gen_tcp:recv(Socket, Size-16, Timeout) of
                {ok, Payload} ->
                    {ok, {type_name(TypeId), Id, Payload}};
                {error, Reason1} ->
                    {error, Reason1}
            end;
        {error, Reason2} ->
            {error, Reason2}
    end.

%%=============================================================================
%% Pure functions
%%=============================================================================

%% @doc Construct packet for init_req
-spec construct_init_req(HostPort, ProcessName) -> Packet when
      HostPort :: hostport(),
      ProcessName :: service(),
      Packet :: iodata().
construct_init_req(HostPort, ProcessName) ->
    OTP = list_to_binary(erlang:system_info(otp_release)),
    Headers = [
               {<<"host_port">>, HostPort},
               {<<"process_name">>, ProcessName},
               {<<"tchannel_language">>, <<"erlang">>},
               {<<"tchannel_language_version">>, OTP},
               {<<"tchannel_version">>, ?TCHANNEL_LIB_VERSION}
              ],
    Payload =
    [
     <<?PROTOCOL_VERSION:16, (length(Headers)):16>>,
     <<
       <<(iolist_size(K)):16, K/binary,
         (iolist_size(V)):16, V/binary
       >> || {K, V} <- Headers
     >>
    ],
    construct_packet(init_req, 0, Payload).


%% @doc Parse init_res. Return tchannel version and headers of the session.
-spec parse_init_res(Payload) -> {Version, Headers} when
      Payload :: binary(),
      Version :: pos_integer(),
      Headers :: [{binary(), binary()}].
parse_init_res(Payload) ->
    <<Version:16, Rest/binary>> = Payload,
    {Headers, <<>>} = parse_headers(16, Rest),
    {Version, Headers}.

-spec construct_call_req(PacketId, TTL, Service, Headers, Args) ->
    {Packet, PacketId} when
      PacketId :: packet_id(),
      TTL :: pos_integer() | undefined,
      Service :: service(),
      Headers :: [transport_header()],
      Args :: {iodata(), iodata(), iodata()},
      Packet :: iodata(),
      PacketId :: packet_id().
construct_call_req(PacketId, TTL, Service, Headers, {Arg1, Arg2, Arg3}) ->
    Headers1 = transport_headers(Headers),
    Payload = [
     <<
       0:8,                               % XXX flags no fragmentation support
       TTL:32,                            % ttl
       0:200,                             % XXX tracing not supported
       (size(Service)):8, Service/binary  % service
     >>, Headers1, <<                     % headers
       0:8                                % XXX csumtype not supported
     >>,
     <<(iolist_size(Arg1)):16>>, Arg1,
     <<(iolist_size(Arg2)):16>>, Arg2,
     <<(iolist_size(Arg3)):16>>, Arg3
    ],
    Packet = construct_packet(call_req, PacketId, Payload),
    {Packet, next_packet_id(PacketId)}.

%% @doc Handle incoming parts of the stream.
%%
%% First two bytes of the packet are size of the full packet, including the 2B
%% of size. All packet (including length) is stored in the buffer.  From the
%% protocol we know the smallest possible packet is 16B, so we can ignore
%% case length = 2.
%%
%% Tip for tchannel v3: subtract include size of the size packet. I.e.
%% PacketSizeInTChannelV3 := PacketSizeInTChannelV2 - 2.
-spec stream_recv(Msg, {Packets, Buffer, Remaining}) ->
    {Packets, Buffer, Remaining} when
      Msg :: binary(),
      Packets :: [binary()],
      Buffer :: binary(),
      Remaining :: undefined | pos_integer().
%% Exit clause.
stream_recv(<<>>, {Acc, Buffer, RemB}) ->
    {Acc, Buffer, RemB};

%% First byte only.
stream_recv(<<FirstByte:8>>, {Acc, <<>>, undefined}) ->
    stream_recv(<<>>, {Acc, <<FirstByte:8>>, undefined});
%% Have first byte, getting the second byte and possibly more.
stream_recv(<<SecondByte:8, Rest/binary>>, {Acc, <<FirstByte:8>>, undefined}) ->
    <<PacketLen:16>> = <<FirstByte:8, SecondByte:8>>,
    stream_recv(Rest, {Acc, <<PacketLen:16>>, PacketLen-2});

%% First 2B or more.
stream_recv(<<PacketLength:16, Rest/binary>>, {Acc, _Buffer, undefined}) ->
    stream_recv(Rest, {Acc, <<PacketLength:16>>, PacketLength-2});

%% Receiving not enough remaining bytes for the packet.
stream_recv(Msg, {Acc, Buffer, RemB}) when size(Msg) < RemB ->
    stream_recv(<<>>, {Acc, <<Buffer/binary, Msg/binary>>, RemB - size(Msg)});

%% Enough for the packet, might have a part of the next one.
stream_recv(Msg, {Acc, Buf, RemB}) when size(Msg) >= RemB ->
    <<Remaining:RemB/binary, Rest/binary>> = Msg,
    Acc2 = [<<Buf/binary, Remaining/binary>>|Acc],
    stream_recv(Rest, {Acc2, <<>>, undefined}).

%%=============================================================================
%% Internal helpers
%%=============================================================================

%% @doc Construct transport headers from [transport_header()].
-spec transport_headers(Headers) -> Payload when
      Headers :: [transport_header()],
      Payload :: iodata().
transport_headers(Headers) ->
    [
     <<(length(Headers)):8>>,
     <<
       <<(iolist_size(bin(K))):8, (bin(K))/binary,
         (iolist_size(bin(V))):8, (bin(V))/binary
       >> || {K, V} <- Headers
     >>
    ].

%% @doc Construct a tchannel packet.
%%
%% Size must be <= 2^16, otherwise it will silently trim the size. Streaming
%% should be done at higher level.
-spec construct_packet(Type, Id, Payload) -> Packet when
      Type :: packet_type(),
      Id :: packet_id(),
      Payload :: iodata(),
      Packet :: iodata().
construct_packet(Type, Id, Payload) ->
    Size = iolist_size(Payload) + 16,
    TypeNum = type_num(Type),
    [<<Size:16,
      TypeNum:8,
      0:8,  % reserved byte
      Id:32,
      0:64>>, % 8 reserved bytes
     Payload].


-spec next_packet_id(packet_id()) -> packet_id().
next_packet_id(16#fffffffe) -> 0;
next_packet_id(X) -> X + 1.


%% @doc Numeric tchannel packet type.
-spec type_num(packet_type()) -> packet_type_no().
type_num(init_req) ->                16#01;
%type_num(init_res) ->               16#02;
type_num(call_req) ->                16#03.
%type_num(call_res) ->               16#04;
%type_num(call_req_continue) ->      16#13;
%type_num(call_res_continue) ->      16#14;
%type_num(cancel) ->                 16#c0;
%type_num(claim) ->                  16#c1;
%type_num(ping_req) ->               16#d0;
%type_num(ping_res) ->               16#d1;
%type_num(error) ->                  16#ff.

-spec bin(atom() | binary()) -> binary().
bin(X) when is_atom(X) -> atom_to_binary(X, utf8);
bin(X) when is_binary(X) -> X.

%% @doc Given a binary and header size size (bits), parse them and return rest.
-spec parse_headers(Bits, Binary) -> {Headers, Rest} when
      Bits :: 8 | 16,
      Binary :: binary(),
      Rest :: binary(), %% remaining binary after parsing header
      Headers :: [{binary(), binary()}].
parse_headers(Bits, Binary) ->
    <<NH:Bits, Rest/binary>> = Binary,
    lists:mapfoldl(
      fun(_, Rest1) ->
              {Key, Rest2} = parse_sized_item(Bits, Rest1),
              {Val, Rest3} = parse_sized_item(Bits, Rest2),
              {{Key, Val}, Rest3}
      end,
      Rest,
      lists:seq(1, NH)
     ).

parse_sized_item(Bits, Payload) ->
    <<Len:Bits, Rest/binary>> = Payload,
    <<Value:Len/binary, Rest1/binary>> = Rest,
    {Value, Rest1}.

%% @doc Parse an incoming packet.
-spec parse_full_packet(Packet) -> {Type, Id, Payload} when
      Packet :: binary(),
      Type :: packet_type(),
      Id :: packet_id(),
      Payload :: binary().
parse_full_packet(<<_Len:16, Type:8, _:8, Id:32, _:64, Payload/binary>>) ->
    {type_name(Type), Id, Payload}.

-spec parse_call_res(Payload) ->
    {Flags, Code, Tracing, Headers, Checksum, Args} when
      Payload :: binary(),
      Flags :: 0, % | 1 | 2,
      Code :: 0 | 1, % ok | error
      Tracing :: undefined, % not supported
      Headers :: [{binary(), binary()}],
      Checksum :: undefined,
      Args :: {binary(), binary(), binary()}.
parse_call_res(<<0:8, Code:8, _Tracing:200, Rest/binary>>) ->
    {Headers, Rest1} = parse_headers(8, Rest),
    Rest2 = case Rest1 of
                <<0:8, Rest1a/binary>> -> Rest1a
                %<<_:40, Rest1a/binary>> -> Rest1a
            end,
    {Arg1, Rest3} = parse_sized_item(16, Rest2),
    {Arg2, Rest4} = parse_sized_item(16, Rest3),
    {Arg3, <<>>}  = parse_sized_item(16, Rest4),
    {0, Code, undefined, Headers, undefined, {Arg1, Arg2, Arg3}}.

-spec type_name(packet_type_no()) -> packet_type().
%type_name(16#01) -> init_req;
type_name(16#02) -> init_res;
%type_name(16#03) -> call_req;
type_name(16#04) -> call_res.
%type_name(16#13) -> call_req_continue;
%type_name(16#14) -> call_res_continue;
%type_name(16#c0) -> cancel;
%type_name(16#c1) -> claim;
%type_name(16#d0) -> ping_req;
%type_name(16#d1) -> ping_res;
%type_name(16#ff) -> error.
