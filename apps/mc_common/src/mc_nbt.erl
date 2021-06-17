-module(mc_nbt).

-export([decode/1, encode/1]).

%
% NBT tags are translated to arrays of tuples (like proplists)
% For example, the NBT tag below
%
% TAG_Compound("hello world") {
%   TAG_String("name"): "Bananrama"
% }
%
% would become in Erlang after decoding
%
% {tag_compound, <<"hello world">>, [
%   {tag_string, <<"name">>, <<"Bananrama">>}
% ]}
%
%

-define(TAG_End, 0).
-define(TAG_Byte, 1).
-define(TAG_Short, 2).
-define(TAG_Int, 3).
-define(TAG_Long, 4).
-define(TAG_Float, 5).
-define(TAG_Double, 6).
-define(TAG_Byte_Array, 7).
-define(TAG_String, 8).
-define(TAG_List, 9).
-define(TAG_Compound, 10).
-define(TAG_Int_Array, 11).
-define(TAG_Long_Array, 12).

-type nbt_tag() ::
    tag_byte
    | tag_short
    | tag_int
    | tag_long
    | tag_float
    | tag_double
    | tag_byte_array
    | tag_string
    | tag_list
    | tag_compound
    | tag_int_array
    | tag_long_array.

-type tag_byte() :: {tag_byte, Name :: binary(), Value :: integer()}.
-type tag_short() :: {tag_short, Name :: binary(), Value :: integer()}.
-type tag_int() :: {tag_int, Name :: binary(), Value :: integer()}.
-type tag_long() :: {tag_long, Name :: binary(), Value :: integer()}.
-type tag_float() :: {tag_float, Name :: binary(), Value :: float()}.
-type tag_double() :: {tag_double, Name :: binary(), Value :: float()}.
-type tag_byte_array() :: {tag_byte_array, Name :: binary(), Value :: binary()}.
-type tag_list() :: {tag_list, Name :: binary(), {ElemT :: nbt_tag(), Values :: [any()]}}.
-type tag_string() :: {tag_string, Name :: binary(), Value :: binary()}.
-type tag_compound() :: {tag_compound, Name :: binary(), Children :: [nbt()]}.
-type tag_int_array() :: {tag_int_array, Name :: binary(), Value :: [integer()]}.
-type tag_long_array() :: {tag_long_array, Name :: binary(), Value :: [integer()]}.

-type nbt() ::
    tag_byte()
    | tag_short()
    | tag_int()
    | tag_long()
    | tag_float()
    | tag_double()
    | tag_byte_array()
    | tag_list()
    | tag_string()
    | tag_compound()
    | tag_int_array()
    | tag_long_array().

-spec decode(binary()) -> {nbt(), binary()}.
decode(<<NumType:8/integer, NameSize:16/unsigned-integer, Name:NameSize/binary, Rest/binary>>) ->
    Type = decode_type(NumType),
    {V, Remainder} = decode_value(Type, Rest),
    {{Type, Name, V}, Remainder}.

-spec decode_type(non_neg_integer()) -> nbt_tag().
decode_type(?TAG_Byte) -> tag_byte;
decode_type(?TAG_Short) -> tag_short;
decode_type(?TAG_Int) -> tag_int;
decode_type(?TAG_Long) -> tag_long;
decode_type(?TAG_Float) -> tag_float;
decode_type(?TAG_Double) -> tag_double;
decode_type(?TAG_Byte_Array) -> tag_byte_array;
decode_type(?TAG_String) -> tag_string;
decode_type(?TAG_List) -> tag_list;
decode_type(?TAG_Compound) -> tag_compound;
decode_type(?TAG_Int_Array) -> tag_int_array;
decode_type(?TAG_Long_Array) -> tag_long_array.

-spec decode_value(nbt_tag(), binary()) -> {term(), binary()}.
decode_value(tag_byte, <<V:8/signed-integer, Rest/binary>>) ->
    {V, Rest};
decode_value(tag_short, <<V:16/signed-integer, Rest/binary>>) ->
    {V, Rest};
decode_value(tag_int, <<V:32/signed-integer, Rest/binary>>) ->
    {V, Rest};
decode_value(tag_long, <<V:64/signed-integer, Rest/binary>>) ->
    {V, Rest};
decode_value(tag_float, <<V:32/float, Rest/binary>>) ->
    {V, Rest};
decode_value(tag_double, <<V:64/float, Rest/binary>>) ->
    {V, Rest};
decode_value(tag_string, <<Len:16/unsigned-integer, V:Len/binary, Rest/binary>>) ->
    {V, Rest};
decode_value(tag_byte_array, <<Len:32/signed-integer, Rest/binary>>) ->
    decode_array(8, Len, Rest);
decode_value(tag_int_array, <<Len:32/signed-integer, Rest/binary>>) ->
    decode_array(32, Len, Rest);
decode_value(tag_long_array, <<Len:32/signed-integer, Rest/binary>>) ->
    decode_array(64, Len, Rest);
decode_value(tag_list, <<LNumType:8, Len:32/signed-integer, Rest/binary>>) ->
    LType = decode_type(LNumType),
    {Lst, Remaining} = decode_list(LType, Rest, Len),
    {{LType, Lst}, Remaining};
decode_value(tag_compound, Bin) ->
    decode_compound(Bin).

decode_array(IntSize, Len, Bin) ->
    <<IntBin:Len/binary, Rest/binary>> = Bin,
    {[X || <<X:IntSize/big-signed-integer>> <= IntBin], Rest}.

decode_list(_Type, Rest, 0) ->
    {[], Rest};
decode_list(Type, Bin, Len) when Len > 0 ->
    {V, Rest} = decode_value(Type, Bin),
    {Vs, Remaining} = decode_list(Type, Rest, Len - 1),
    {[V | Vs], Remaining}.

decode_compound(<<?TAG_End, Rest/binary>>) ->
    {[], Rest};
decode_compound(Bin) ->
    {Tag, Rest} = decode(Bin),
    {Tags, Remaining} = decode_compound(Rest),
    {[Tag | Tags], Remaining}.

-spec encode(nbt()) -> binary().
encode({Type, Name, V}) ->
    TypeCode = encode_type(Type),
    BinV = encode_value(TypeCode, V),
    <<TypeCode:8, (byte_size(Name)):16/unsigned-integer, Name/binary, BinV/binary>>.

encode_type(tag_byte) -> ?TAG_Byte;
encode_type(tag_short) -> ?TAG_Short;
encode_type(tag_int) -> ?TAG_Int;
encode_type(tag_long) -> ?TAG_Long;
encode_type(tag_float) -> ?TAG_Float;
encode_type(tag_double) -> ?TAG_Double;
encode_type(tag_byte_array) -> ?TAG_Byte_Array;
encode_type(tag_string) -> ?TAG_String;
encode_type(tag_list) -> ?TAG_List;
encode_type(tag_compound) -> ?TAG_Compound;
encode_type(tag_int_array) -> ?TAG_Int_Array;
encode_type(tag_long_array) -> ?TAG_Long_Array.

encode_value(?TAG_Byte, V) -> <<V:8/integer>>;
encode_value(?TAG_Short, V) -> <<V:16/integer>>;
encode_value(?TAG_Int, V) -> <<V:32/integer>>;
encode_value(?TAG_Long, V) -> <<V:64/integer>>;
encode_value(?TAG_Float, V) -> <<V:32/float>>;
encode_value(?TAG_Double, V) -> <<V:64/float>>;
encode_value(?TAG_String, V) -> <<(byte_size(V)):16/signed-integer, V/binary>>;
encode_value(?TAG_Byte_Array, V) -> encode_array(8, V);
encode_value(?TAG_Int_Array, V) -> encode_array(32, V);
encode_value(?TAG_Long_Array, V) -> encode_array(64, V);
encode_value(?TAG_List, {ListType, Lst}) -> encode_list(ListType, Lst);
encode_value(?TAG_Compound, Lst) -> <<(encode_compound(Lst))/binary, ?TAG_End>>.

encode_array(IntSize, Lst) ->
    EncArray = <<<<X:IntSize/integer>> || X <- Lst>>,
    <<(length(Lst)):32/signed-integer, EncArray/binary>>.

encode_list(ListType, Lst) ->
    Type = encode_type(ListType),
    EncLst = <<<<(encode_value(Type, X))/binary>> || X <- Lst>>,
    <<Type:8, (length(Lst)):32/signed-integer, EncLst/binary>>.

encode_compound(Lst) ->
    <<<<(encode(X))/binary>> || X <- Lst>>.
