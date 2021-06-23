-module(mc_protocol).

-export([
    init/0,
    init/1,
    version/1,
    decode_id/4,
    encode_name/3,
    decode/3,
    encode/3
]).

-type version() :: non_neg_integer().
-type packet_name() :: atom().
-type packet_id() :: non_neg_integer().
-type packet_state() :: handshaking | status | login | play.
-type packet_dir() :: serverbound | clientbound.
-type field_name() :: atom().
-type field_type() ::
    varint
    | varlong
    | string
    | u8
    | i8
    | u16
    | i16
    | u32
    | i32
    | u64
    | i64
    | float
    | double
    | uuid
    | nbt
    | rest
    | position
    | {binary, LenT :: field_type()}
    | {array, LenT :: field_type(), ValT :: field_type()}
    | {bool, ValT :: field_type()}
    | {bitmask, ValT :: field_type(), [{Val :: term(), BitPos :: integer()}]}
    | {enum, ValT :: field_type(), [{Key :: atom(), Val :: term()}]}
    | {packet, packet_def()}
    | angle.

-type packet_def() :: {field_name(), field_type()}.

-export_type([
    packet_name/0,
    packet_dir/0,
    packet_state/0,
    packet_id/0,
    field_type/0,
    version/0
]).

-record(proto, {
    protocol_mod :: module()
}).

-type protocol() :: #proto{}.

-spec init() -> protocol().
init() ->
    ProtocolMod = protocol_1_16_4,
    init(ProtocolMod).

-spec init(module()) -> protocol().
init(ProtocolMod) ->
    #proto{protocol_mod = ProtocolMod}.

-spec version(protocol()) -> version().
version(#proto{protocol_mod = Mod}) ->
    Mod:version().

-spec decode_id(packet_id(), packet_state(), packet_dir(), protocol()) ->
    packet_name() | {unknown_packet, term()}.
decode_id(ID, State, Dir, #proto{protocol_mod = Mod}) ->
    Mod:lookup_packet_info({name, {ID, State, Dir}}).

-spec encode_name(packet_name(), packet_dir(), protocol()) ->
    packet_id() | {unknown_packet, term()}.
encode_name(Name, Dir, #proto{protocol_mod = Mod}) ->
    Mod:lookup_packet_info({id, {Name, Dir}}).

lookup_packet_fields(Name, #proto{protocol_mod = Mod}) ->
    Mod:packet(Name).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Decoding packets
%
% Packets IDs are not unique, only the tuple of the ID, state, and direction are.
% For example, you need {0x01, login, serverbound} to know that packet ID 0x01 corresponds
% to the Encryption Response packet. Protocol version is also needed but should be passed in init.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec decode(Name :: packet_name(), Body :: binary(), Protocol :: protocol()) -> map().
decode(Name, Body, Protocol) ->
    Fields = lookup_packet_fields(Name, Protocol),
    {Decoded, <<>>} = decode_fields(Body, Fields, #{}),
    Decoded.

add_decoded(ignore, _, Decoded) -> Decoded;
add_decoded(_, none, Decoded) -> Decoded;
add_decoded(FName, FValue, Decoded) -> maps:put(FName, FValue, Decoded).

%% Decode a list of fields from the binary.
%% Any field with name 'ignore' will be ignored, and any field with a decoded value of 'none' will be ignored.
-spec decode_fields(Body, FieldDefinitions, DecodedFields) -> {NewDecodedFields, Rest} when
    Body :: binary(),
    Rest :: binary(),
    FieldDefinitions :: [packet_def()],
    DecodedFields :: map(),
    NewDecodedFields :: map().
decode_fields(Rest, [], Decoded) ->
    {Decoded, Rest};
decode_fields(Body, [{FName, FType} | Fields], Decoded) ->
    {FValue, Rest} =
        try
            decode_field(Body, FType)
        catch
            error:Error -> erlang:error({decode_fail, Error}, [Body, Fields, Decoded])
        end,
    decode_fields(Rest, Fields, add_decoded(FName, FValue, Decoded)).

-spec decode_field(binary(), field_type()) -> {Value :: any(), Rest :: binary()}.
decode_field(Body, varint) ->
    mc_varint:decode(Body);
decode_field(Body, varlong) ->
    mc_varint:decode_long(Body);
decode_field(<<V:8/unsigned-integer, Rest/binary>>, u8) ->
    {V, Rest};
decode_field(<<V:16/big-unsigned-integer, Rest/binary>>, u16) ->
    {V, Rest};
decode_field(<<V:32/big-unsigned-integer, Rest/binary>>, u32) ->
    {V, Rest};
decode_field(<<V:64/big-unsigned-integer, Rest/binary>>, u64) ->
    {V, Rest};
decode_field(<<V:8/signed-integer, Rest/binary>>, i8) ->
    {V, Rest};
decode_field(<<V:16/big-signed-integer, Rest/binary>>, i16) ->
    {V, Rest};
decode_field(<<V:32/big-signed-integer, Rest/binary>>, i32) ->
    {V, Rest};
decode_field(<<V:64/big-signed-integer, Rest/binary>>, i64) ->
    {V, Rest};
decode_field(<<1, Rest/binary>>, bool) ->
    {true, Rest};
decode_field(<<0, Rest/binary>>, bool) ->
    {false, Rest};
decode_field(Body, string) ->
    {Len, Rest} = mc_varint:decode(Body),
    <<Val:Len/binary, Rest2/binary>> = Rest,
    {Val, Rest2};
decode_field(<<V:32/signed-float, Rest/binary>>, float) ->
    {V, Rest};
decode_field(<<V:64/signed-float, Rest/binary>>, double) ->
    {V, Rest};
decode_field(<<V:8/unsigned-integer, Rest/binary>>, angle) ->
    {(V / 256) * 360, Rest};
decode_field(Bin, nbt) ->
    mc_nbt:decode(Bin);
decode_field(Bin, rest) ->
    {Bin, <<>>};
decode_field(
    <<X:26/signed-integer, Z:26/signed-integer, Y:12/signed-integer, Rest/binary>>,
    position
) ->
    {{X, Y, Z}, Rest};
decode_field(<<1, Rest/binary>>, {option, Repr}) ->
    decode_field(Rest, Repr);
decode_field(<<0, Rest/binary>>, {option, _Repr}) ->
    {none, Rest};
decode_field(Bin, {option, OptRepr, Options}) ->
    {Opt, Rest} = decode_field(Bin, OptRepr),
    {Opt, OptBodyRepr} = lists:keyfind(Opt, 1, Options),
    {OptBody, Rest2} = decode_field(Rest, OptBodyRepr),
    {{Opt, OptBody}, Rest2};
decode_field(<<V:16/binary, Rest/binary>>, uuid) ->
    {V, Rest};
decode_field(Bin, {enum, Repr, Enum}) ->
    {EncV, Rest} = decode_field(Bin, Repr),
    case lists:keyfind(EncV, 2, Enum) of
        {DecAtom, _} ->
            {DecAtom, Rest};
        false ->
            erlang:error({invalid_enum, EncV, Enum})
    end;
decode_field(Bin, {bitmask, Repr, Enum}) ->
    {V, Rest} = decode_field(Bin, Repr),
    Flags = [Flag || {Flag, Pos} <- Enum, V band (1 bsl Pos) > 0],
    {Flags, Rest};
decode_field(Bin, {binary, LenRepr}) ->
    {Len, Rest} = decode_field(Bin, LenRepr),
    <<V:Len/binary, Rest2/binary>> = Rest,
    {V, Rest2};
decode_field(Bin, {array, LenRepr, ElRepr}) ->
    {Len, Rest} = decode_field(Bin, LenRepr),
    decode_array(Rest, ElRepr, Len);
decode_field(Bin, {packet, Fields}) ->
    decode_fields(Bin, Fields, #{}).

decode_array(Bin, _Repr, 0) ->
    {[], Bin};
decode_array(Bin, Repr, Len) when Len > 0 ->
    {V, Rest} = decode_field(Bin, Repr),
    {Vs, Remaining} = decode_array(Rest, Repr, Len - 1),
    {[V | Vs], Remaining}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Encoding packets
%
% Only the packet name is needed (along with the protocol version) to encode a packet.
% For example, a Login Start packet is only ever sent from the server to the client. No packet
% is sent in both directions.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec encode({packet_name(), packet_dir()}, map(), protocol()) -> {packet_id(), binary()}.
encode({Name, Dir}, Packet, Protocol) ->
    ID = encode_name(Name, Dir, Protocol),
    Body = encode_packet_body(Name, Packet, Protocol),
    {ID, Body}.

encode_packet_body(Name, Packet, Protocol) ->
    Fields = lookup_packet_fields(Name, Protocol),
    encode_fields(Fields, Packet, <<>>).

encode_fields([{FName, FType} | Fields], Packet, Acc) ->
    FValue = maps:get(FName, Packet, {missing, FName}),
    EncField = encode_field(FType, FValue),
    encode_fields(Fields, Packet, <<Acc/binary, EncField/binary>>);
encode_fields([], _Packet, BinAcc) ->
    BinAcc.

-spec encode_field(field_type(), V :: any()) -> Encoded :: binary().
encode_field(varint, V) ->
    mc_varint:encode(V);
encode_field(varlong, V) ->
    mc_varint:encode_long(V);
encode_field(u8, V) ->
    <<V:8/big-unsigned-integer>>;
encode_field(u16, V) ->
    <<V:16/big-unsigned-integer>>;
encode_field(u32, V) ->
    <<V:32/big-unsigned-integer>>;
encode_field(u64, V) ->
    <<V:64/big-unsigned-integer>>;
encode_field(i8, V) ->
    <<V:8/big-signed-integer>>;
encode_field(i16, V) ->
    <<V:16/big-signed-integer>>;
encode_field(i32, V) ->
    <<V:32/big-signed-integer>>;
encode_field(i64, V) ->
    <<V:64/big-signed-integer>>;
encode_field(bool, true) ->
    <<1>>;
encode_field(bool, false) ->
    <<0>>;
encode_field(string, V) ->
    Len = mc_varint:encode(byte_size(V)),
    <<Len/binary, V/binary>>;
encode_field(float, V) ->
    <<V:32/big-float>>;
encode_field(double, V) ->
    <<V:64/big-float>>;
encode_field(angle, V) ->
    Angle = round(((round(V) rem 360) / 360) * 255),
    <<Angle:8/unsigned-integer>>;
encode_field(nbt, V) ->
    mc_nbt:encode(V);
encode_field(uuid, V) when byte_size(V) =:= 16 ->
    V;
encode_field(rest, V) when is_binary(V) ->
    V;
encode_field(position, {X, Y, Z}) ->
    <<X:26/signed-integer, Z:26/signed-integer, Y:12/signed-integer>>;
encode_field({option, _Repr}, {missing, _FName}) ->
    encode_field(bool, false);
encode_field({option, Repr}, V) ->
    <<(encode_field(bool, true))/binary, (encode_field(Repr, V))/binary>>;
encode_field({option, OptRepr, Options}, {Opt, OptBody}) ->
    OptBin = encode_field(OptRepr, Opt),
    {Opt, OptBodyRepr} = lists:keyfind(Opt, 1, Options),
    OptBodyBin = encode_field(OptBodyRepr, OptBody),
    <<OptBin/binary, OptBodyBin/binary>>;
encode_field({enum, Repr, Enum}, V) ->
    {_, EncV} = lists:keyfind(V, 1, Enum),
    encode_field(Repr, EncV);
encode_field({bitmask, Repr, Enum}, Flags) ->
    BitsToSet = [1 bsl Pos || Flag <- Flags, {_, Pos} <- [lists:keyfind(Flag, 1, Enum)]],
    encode_field(Repr, lists:sum(BitsToSet));
encode_field({binary, LenRepr}, V) ->
    LenBin = encode_field(LenRepr, byte_size(V)),
    <<LenBin/binary, V/binary>>;
encode_field({array, LenRepr, ElRepr}, Vs) ->
    LenBin = encode_field(LenRepr, length(Vs)),
    VsBin = <<<<(encode_field(ElRepr, X))/binary>> || X <- Vs>>,
    <<LenBin/binary, VsBin/binary>>;
encode_field({packet, Fields}, Packet) ->
    encode_fields(Fields, Packet, <<>>).
