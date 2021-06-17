-module(mc_varint).

-include_lib("eunit/include/eunit.hrl").

-export([decode/1, decode_long/1, encode/1, encode_long/1]).

truncate(Bin, Size) ->
    Leftover = max(bit_size(Bin) - Size, 0),
    <<_:Leftover, Bin2/bitstring>> = Bin,
    Bin2.

truncate_test_() ->
    [
        ?_assert(<<>> =:= truncate(<<>>, 12)),
        ?_assert(<<12:6>> =:= truncate(<<12>>, 6)),
        ?_assert(<<12:6>> =:= truncate(<<12:6>>, 12)),
        ?_assert(<<>> =:= truncate(<<12:6>>, 0))
    ].

pad(Bin, Size) ->
    Needed = max(Size - bit_size(Bin), 0),
    <<0:Needed, Bin/bitstring>>.

pad_test_() ->
    [
        ?_assert(<<12>> =:= pad(<<12:6>>, 8)),
        ?_assert(<<12>> =:= pad(<<12>>, 6))
    ].

-spec decode(binary()) -> {integer(), binary()} | needmore.
decode(Bin) -> decode(Bin, 5).

-spec decode_long(binary()) -> {integer(), binary()}.
decode_long(Bin) -> decode(Bin, 10).

decode(Bin, MaxBytes) ->
    MaxBits = MaxBytes * 7,
    case take_varint(Bin, <<>>) of
        {ok, Bytes, Rest} ->
            if
                bit_size(Bytes) =< MaxBits ->
                    % A varint can be 0-35/0-70 bits, so we pad & truncate to IntSize (32 or 64 bits)
                    IntSize = MaxBits - (MaxBits rem 8),
                    <<X:IntSize/signed-integer>> = truncate(pad(Bytes, IntSize), IntSize),
                    {X, Rest};
                true ->
                    error(overflow)
            end;
        needmore ->
            needmore
    end.

take_varint(<<>>, _) ->
    needmore;
take_varint(<<0:1, V:7, Rest/bitstring>>, Acc) ->
    {ok, <<V:7, Acc/bitstring>>, Rest};
take_varint(<<1:1, V:7, Rest/bitstring>>, Acc) ->
    take_varint(Rest, <<V:7, Acc/bitstring>>).

decode_varint_test_() ->
    [
        % varint
        ?_assert({0, <<>>} =:= decode(<<0>>)),
        ?_assert({1, <<>>} =:= decode(<<1>>)),
        ?_assert({2, <<>>} =:= decode(<<2>>)),
        ?_assert({127, <<>>} =:= decode(<<127>>)),
        ?_assert({128, <<>>} =:= decode(<<128, 1>>)),
        ?_assert({255, <<>>} =:= decode(<<255, 1>>)),
        ?_assert({2097151, <<>>} =:= decode(<<255, 255, 127>>)),
        ?_assert({2147483647, <<>>} =:= decode(<<255, 255, 255, 255, 7>>)),
        ?_assert({-1, <<>>} =:= decode(<<255, 255, 255, 255, 15>>)),
        ?_assert({-2147483648, <<>>} =:= decode(<<128, 128, 128, 128, 8>>)),

        % varlong
        ?_assert({0, <<>>} =:= decode_long(<<0>>)),
        ?_assert({1, <<>>} =:= decode_long(<<1>>)),
        ?_assert({2, <<>>} =:= decode_long(<<2>>)),
        ?_assert({127, <<>>} =:= decode_long(<<127>>)),
        ?_assert({128, <<>>} =:= decode_long(<<128, 1>>)),
        ?_assert({255, <<>>} =:= decode_long(<<255, 1>>)),
        ?_assert({2097151, <<>>} =:= decode_long(<<255, 255, 127>>)),
        ?_assert({2147483647, <<>>} =:= decode_long(<<255, 255, 255, 255, 7>>)),
        ?_assert(
            {9223372036854775807, <<>>} =:=
                decode_long(<<255, 255, 255, 255, 255, 255, 255, 255, 127>>)
        ),
        ?_assert({-1, <<>>} =:= decode_long(<<255, 255, 255, 255, 255, 255, 255, 255, 255, 1>>)),
        ?_assert(
            {-2147483648, <<>>} =:= decode_long(<<128, 128, 128, 128, 248, 255, 255, 255, 255, 1>>)
        ),
        ?_assert(
            {-9223372036854775808, <<>>} =:=
                decode_long(<<128, 128, 128, 128, 128, 128, 128, 128, 128, 1>>)
        ),

        % leftover
        ?_assert({0, <<255>>} =:= decode(<<0, 255>>)),

        % invalid
        ?_assertError(overflow, decode(<<255, 255, 255, 255, 255, 16>>)),
        ?_assertEqual(needmore, decode(<<255, 255>>))
    ].

-spec encode(integer()) -> binary().
encode(V) ->
    encode(V, 4).

-spec encode_long(integer()) -> binary().
encode_long(V) ->
    encode(V, 8).

encode(V, MaxBytes) ->
    if
        V >= 0 ->
            encode_varint(V);
        true ->
            encode_neg_varint(V, MaxBytes)
    end.

-spec encode_varint(integer()) -> binary().
encode_varint(I) when is_integer(I), I >= 0, I =< 127 ->
    <<I>>;
encode_varint(I) when is_integer(I), I > 127 ->
    <<1:1, (I band 127):7, (encode_varint(I bsr 7))/binary>>.

encode_neg_varint(I, MaxBytes) ->
    % we do a "C" cast of the negative integer to an unsigned one, and encode that
    <<V:(MaxBytes * 8)/unsigned-integer>> = <<I:(MaxBytes * 8)/integer>>,
    encode_varint(V).

encode_varint_test_() ->
    [
        % varint
        ?_assert(encode(0) =:= <<0>>),
        ?_assert(encode(1) =:= <<1>>),
        ?_assert(encode(2) =:= <<2>>),
        ?_assert(encode(127) =:= <<127>>),
        ?_assert(encode(128) =:= <<128, 1>>),
        ?_assert(encode(255) =:= <<255, 1>>),
        ?_assert(encode(2097151) =:= <<255, 255, 127>>),
        ?_assert(encode(2147483647) =:= <<255, 255, 255, 255, 7>>),
        ?_assert(encode(-1) =:= <<255, 255, 255, 255, 15>>),
        ?_assert(encode(-2147483648) =:= <<128, 128, 128, 128, 8>>)
    ].
