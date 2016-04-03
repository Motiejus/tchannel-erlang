%%% @doc Pure packet creator and parser for tchannel_conn.
%%%
-module(tchannel_packet).


-export([construct_init_req/0,
         parse_init_res/1,
         construct_call_req/5]).

-include("types.hrl").
-include("consts.hrl").

-ifdef(TEST).
-export([next_packet_id/1]).
-endif.

%% @doc Construct packet for init_req
-spec construct_init_req() -> Packet when Packet :: iodata().
construct_init_req() ->
    OTP = list_to_binary(erlang:system_info(otp_release)),
    Headers = [
               {<<"host_port">>, <<"0.0.0.0:0">>},
               {<<"process_name">>, <<"ringpoprop">>},
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
    <<Version:16, NH:16, Rest/binary>> = Payload,
    Headers = parse_headers(Rest, NH),
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

-spec parse_headers(Binary, NH) -> [{Key, Value}] when
      Binary :: binary(),
      NH :: pos_integer(),
      Key :: binary(),
      Value :: binary().
parse_headers(Binary, NH) ->
    {Headers, <<>>} = lists:mapfoldl(
                        fun(_, Rest1) ->
                                {Key, Rest2} = parse_header_item(Rest1),
                                {Val, Rest3} = parse_header_item(Rest2),
                                {{Key, Val}, Rest3}
                        end,
                        Binary,
                        lists:seq(1, NH)
                       ),
    Headers.

parse_header_item(<<Len:16, Rest/binary>>) ->
    <<Value:Len/binary, Rest1/binary>> = Rest,
    {Value, Rest1}.
