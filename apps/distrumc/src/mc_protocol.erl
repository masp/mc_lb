-module(mc_protocol).

-include_lib("eunit/include/eunit.hrl").

-export([
    init/0,
    lookup_packet_name/4,
    decode_packet/3,
    encode_packet/3,
    decode_field/2,
    encode_field/2,
    encode_fields/2
]).

% WARNING: Used for testing, do not directly use
-export([lookup_id/2, lookup_name/3, packet/1]).

-type packet_id() :: non_neg_integer().
-type packet_state() :: handshaking | status | login | play.
-type packet_dir() :: serverbound | clientbound.
-type packet_field_type() ::
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
    | binary
    | array
    | bool
    | bitmask
    | enum
    | packet
    | angle.

-type packet_name() :: atom().
-type packet_field_def() :: {atom() | packet_field_type()}.

-record(state, {
    protocol_mod :: module()
}).

-type packet_protocol() :: #state{}.

-spec init() -> packet_protocol().
init() ->
    ProtocolMod = protocol_1_16_4,
    init(ProtocolMod).

init(ProtocolMod) ->
    #state{protocol_mod = ProtocolMod}.

-spec lookup_packet_name(packet_id(), packet_state(), packet_dir(), packet_protocol()) ->
    packet_name().
lookup_packet_name(ID, State, Dir, #state{protocol_mod = Mod}) ->
    Mod:lookup_name(ID, State, Dir).

lookup_packet_id(Name, Dir, #state{protocol_mod = Mod}) ->
    Mod:lookup_id(Name, Dir).

lookup_packet_fields(Name, #state{protocol_mod = Mod}) ->
    Mod:packet(Name).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Decoding packets
%
% Packets IDs are not unique, only the tuple of the ID, state, and direction are.
% For example, you need {0x01, login, serverbound} to know that packet ID 0x01 corresponds
% to the Encryption Response packet. Protocol version is also needed but should be passed in init.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
decode_packet(Name, Body, Protocol) ->
    Fields = lookup_packet_fields(Name, Protocol),
    {Decoded, <<>>} = decode_fields(Body, Fields, #{}),
    Decoded.

add_decoded(ignore, _, Decoded) -> Decoded;
add_decoded(_, none, Decoded) -> Decoded;
add_decoded(FName, FValue, Decoded) -> maps:put(FName, FValue, Decoded).

%% Decode a list of fields from the binary. Any field with name 'ignore' will be ignored.
-spec decode_fields(binary(), [packet_field_def()], map()) -> map().
decode_fields(Rest, [], Decoded) ->
    {Decoded, Rest};
decode_fields(Body, [{FName, FType} | Fields], Decoded) ->
    {FValue, Rest} = decode_field(Body, FType),
    decode_fields(Rest, Fields, add_decoded(FName, FValue, Decoded)).

-spec decode_field(binary(), term()) -> any().
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
decode_field(<<V:128/binary, Rest/binary>>, uuid) ->
    {V, Rest};
decode_field(Bin, {enum, Repr, Enum}) ->
    {EncV, Rest} = decode_field(Bin, Repr),
    {DecAtom, _} = lists:keyfind(EncV, 2, Enum),
    {DecAtom, Rest};
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

-spec encode_packet({packet_dir(), packet_name()}, map(), packet_protocol()) -> binary().
encode_packet({Dir, Name}, Packet, Protocol) ->
    ID = lookup_packet_id(Name, Dir, Protocol),
    Body = encode_packet_body(Name, Packet, Protocol),
    {ID, Body}.

encode_packet_body(Name, Packet, Protocol) ->
    Fields = lookup_packet_fields(Name, Protocol),
    encode_fields(Fields, Packet, <<>>).

encode_fields(Fields, Packet) -> encode_fields(Fields, Packet, <<>>).

encode_fields([{FName, FType} | Fields], Packet, Acc) ->
    FValue = maps:get(FName, Packet, {missing, FName}),
    EncField = encode_field(FType, FValue),
    encode_fields(Fields, Packet, <<Acc/binary, EncField/binary>>);
encode_fields([], _Packet, BinAcc) ->
    BinAcc.

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
encode_field(uuid, V) ->
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

%% Fake testing packet IDs and definitions
lookup_id(Name, Dir) ->
    case {Name, Dir} of
        {handshake, clientbound} -> 16#00;
        {stringer, clientbound} -> 16#01;
        {uuider, clientbound} -> 16#02;
        {enumer, clientbound} -> 16#03;
        {arrayer, clientbound} -> 16#04;
        {positioner, clientbound} -> 16#05;
        {nested, clientbound} -> 16#06
    end.

lookup_name(ID, State, Dir) ->
    case {ID, State, Dir} of
        {16#00, handshaking, clientbound} -> handshake;
        {16#01, handshaking, clientbound} -> stringer;
        {16#02, handshaking, clientbound} -> uuider;
        {16#03, handshaking, clientbound} -> enumer;
        {16#04, handshaking, clientbound} -> arrayer;
        {16#05, handshaking, clientbound} -> positioner;
        {16#06, handshaking, clientbound} -> nested
    end.

packet(handshake) ->
    [
        {field0, varint},
        {field1, i16},
        {field2, u32},
        {field3, angle}
    ];
packet(stringer) ->
    [
        {field0, string}
    ];
packet(uuider) ->
    [
        {field0, uuid}
    ];
packet(enumer) ->
    [
        {field0,
            {enum, varint, [
                {none, 0},
                {enum0, 3},
                {enum1, 64}
            ]}},
        {field1,
            {bitmask, varint, [
                {mask0, 0},
                {mask1, 2},
                {mask2, 4}
            ]}},
        {field2, {option, u8}},
        {field3,
            {option,
                {enum, varint, [
                    {opt0, 0},
                    {opt1, 1}
                ]},
                [
                    {opt0, u8},
                    {opt1, u16}
                ]}}
    ];
packet(arrayer) ->
    [
        {field0, {array, varint, u8}},
        {field1, {binary, varint}}
    ];
packet(positioner) ->
    [{field0, position}];
packet(nested) ->
    [
        {field0,
            {array, varint,
                {packet, [
                    {nested0, varint},
                    {nested1, u8}
                ]}}}
    ].

lookup_test_() ->
    Protocol = init(?MODULE),
    [
        ?_assertEqual(stringer, lookup_packet_name(16#01, handshaking, clientbound, Protocol)),
        ?_assertEqual(16#01, lookup_packet_id(stringer, clientbound, Protocol))
    ].

decode_test_() ->
    Protocol = init(?MODULE),
    [
        ?_assertEqual(
            #{field0 => 1, field1 => 5, field2 => 10, field3 => 45.0},
            decode_packet(handshake, <<1, 0, 5, 0, 0, 0, 10, 32>>, Protocol)
        ),
        ?_assertEqual(
            #{field0 => <<"©as"/utf8>>},
            decode_packet(stringer, <<4, 16#C2, 16#A9, 16#61, 16#73>>, Protocol)
        ),
        ?_assertEqual(
            #{field0 => enum1, field1 => [mask1, mask2], field2 => 5, field3 => {opt1, 6}},
            decode_packet(enumer, <<64, 20, 1, 5, 1, 0, 6>>, Protocol)
        ),
        ?_assertEqual(
            #{field1 => [], field3 => {opt0, 3}},
            decode_packet(enumer, <<0, 0, 0, 0, 3>>, Protocol)
        ),
        ?_assertEqual(
            #{field0 => [1, 2, 3, 4], field1 => <<1, 2, 3, 4>>},
            decode_packet(arrayer, <<4, 1, 2, 3, 4, 4, 1, 2, 3, 4>>, Protocol)
        ),
        ?_assertEqual(
            #{field0 => {-1, 2, 3}},
            decode_packet(positioner, <<255, 255, 255, 192, 0, 0, 48, 2>>, Protocol)
        ),
        ?_assertEqual(
            #{field0 => [#{nested0 => 2, nested1 => 5}]},
            decode_packet(nested, <<1, 2, 5>>, Protocol)
        )
    ].

encode_test_() ->
    Protocol = init(?MODULE),
    [
        ?_assertEqual(
            {0, <<5, 255, 255, 0, 0, 0, 10, 32>>},
            encode_packet(
                {clientbound, handshake},
                #{field0 => 5, field1 => -1, field2 => 10, field3 => 45.0},
                Protocol
            )
        ),
        ?_assertEqual(
            {1, <<4, 16#C2, 16#A9, 16#61, 16#73>>},
            encode_packet({clientbound, stringer}, #{field0 => <<"©as"/utf8>>}, Protocol)
        ),
        ?_assertEqual(
            {2, <<247, 248, 249, 250, 251, 252, 253, 254>>},
            encode_packet(
                {clientbound, uuider},
                #{field0 => <<247, 248, 249, 250, 251, 252, 253, 254>>},
                Protocol
            )
        ),
        ?_assertEqual(
            {3, <<3, 20, 0, 1, 0, 7>>},
            encode_packet(
                {clientbound, enumer},
                #{field0 => enum0, field1 => [mask1, mask2], field3 => {opt1, 7}},
                Protocol
            )
        ),
        ?_assertEqual(
            {4, <<4, 1, 2, 3, 4, 4, 1, 2, 3, 4>>},
            encode_packet(
                {clientbound, arrayer},
                #{field0 => [1, 2, 3, 4], field1 => <<1, 2, 3, 4>>},
                Protocol
            )
        ),
        ?_assertEqual(
            {5, <<255, 255, 255, 192, 0, 0, 48, 2>>},
            encode_packet({clientbound, positioner}, #{field0 => {-1, 2, 3}}, Protocol)
        ),
        ?_assertEqual(
            {6, <<1, 2, 5>>},
            encode_packet(
                {clientbound, nested},
                #{field0 => [#{nested0 => 2, nested1 => 5}]},
                Protocol
            )
        )
    ].
