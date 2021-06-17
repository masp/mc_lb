-module(mc_protocol_tests).
-behaviour(protocol_definition).

-include("protocols/protocol_utils.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([version/0, lookup_packet_info/1, packet/1]).

version() -> 1.

lookup_packet_info(PacketInfo) ->
    case PacketInfo of
        ?PACKET(16#00, handshaking, clientbound, handshake);
        ?PACKET(16#01, handshaking, clientbound, stringer);
        ?PACKET(16#02, handshaking, clientbound, uuider);
        ?PACKET(16#03, handshaking, clientbound, enumer);
        ?PACKET(16#04, handshaking, clientbound, arrayer);
        ?PACKET(16#05, handshaking, clientbound, positioner);
        ?PACKET(16#06, handshaking, clientbound, nested)
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
    Protocol = mc_protocol:init(?MODULE),
    [
        ?_assertEqual(
            stringer,
            mc_protocol:decode_id(16#01, handshaking, clientbound, Protocol)
        ),
        ?_assertEqual(16#01, mc_protocol:encode_name(stringer, clientbound, Protocol))
    ].

decode_test_() ->
    Protocol = mc_protocol:init(?MODULE),
    [
        ?_assertEqual(
            #{field0 => 1, field1 => 5, field2 => 10, field3 => 45.0},
            mc_protocol:decode(handshake, <<1, 0, 5, 0, 0, 0, 10, 32>>, Protocol)
        ),
        ?_assertEqual(
            #{field0 => <<"©as"/utf8>>},
            mc_protocol:decode(stringer, <<4, 16#C2, 16#A9, 16#61, 16#73>>, Protocol)
        ),
        ?_assertEqual(
            #{field0 => enum1, field1 => [mask1, mask2], field2 => 5, field3 => {opt1, 6}},
            mc_protocol:decode(enumer, <<64, 20, 1, 5, 1, 0, 6>>, Protocol)
        ),
        ?_assertEqual(
            #{field1 => [], field3 => {opt0, 3}},
            mc_protocol:decode(enumer, <<0, 0, 0, 0, 3>>, Protocol)
        ),
        ?_assertEqual(
            #{
                field0 =>
                    <<247, 248, 249, 250, 251, 252, 253, 254, 247, 248, 249, 250, 251, 252, 253,
                        254>>
            },
            mc_protocol:decode(
                uuider,
                <<247, 248, 249, 250, 251, 252, 253, 254, 247, 248, 249, 250, 251, 252, 253, 254>>,
                Protocol
            )
        ),
        ?_assertEqual(
            #{field0 => [1, 2, 3, 4], field1 => <<1, 2, 3, 4>>},
            mc_protocol:decode(arrayer, <<4, 1, 2, 3, 4, 4, 1, 2, 3, 4>>, Protocol)
        ),
        ?_assertEqual(
            #{field0 => {-1, 2, 3}},
            mc_protocol:decode(positioner, <<255, 255, 255, 192, 0, 0, 48, 2>>, Protocol)
        ),
        ?_assertEqual(
            #{field0 => [#{nested0 => 2, nested1 => 5}]},
            mc_protocol:decode(nested, <<1, 2, 5>>, Protocol)
        )
    ].

encode_test_() ->
    Protocol = mc_protocol:init(?MODULE),
    [
        ?_assertEqual(
            {0, <<5, 255, 255, 0, 0, 0, 10, 32>>},
            mc_protocol:encode(
                {handshake, clientbound},
                #{field0 => 5, field1 => -1, field2 => 10, field3 => 45.0},
                Protocol
            )
        ),
        ?_assertEqual(
            {1, <<4, 16#C2, 16#A9, 16#61, 16#73>>},
            mc_protocol:encode(
                {stringer, clientbound},
                #{field0 => <<"©as"/utf8>>},
                Protocol
            )
        ),
        ?_assertEqual(
            {2, <<247, 248, 249, 250, 251, 252, 253, 254, 247, 248, 249, 250, 251, 252, 253, 254>>},
            mc_protocol:encode(
                {uuider, clientbound},
                #{
                    field0 =>
                        <<247, 248, 249, 250, 251, 252, 253, 254, 247, 248, 249, 250, 251, 252, 253,
                            254>>
                },
                Protocol
            )
        ),
        ?_assertEqual(
            {3, <<3, 20, 0, 1, 0, 7>>},
            mc_protocol:encode(
                {enumer, clientbound},
                #{field0 => enum0, field1 => [mask1, mask2], field3 => {opt1, 7}},
                Protocol
            )
        ),
        ?_assertEqual(
            {4, <<4, 1, 2, 3, 4, 4, 1, 2, 3, 4>>},
            mc_protocol:encode(
                {arrayer, clientbound},
                #{field0 => [1, 2, 3, 4], field1 => <<1, 2, 3, 4>>},
                Protocol
            )
        ),
        ?_assertEqual(
            {5, <<255, 255, 255, 192, 0, 0, 48, 2>>},
            mc_protocol:encode({positioner, clientbound}, #{field0 => {-1, 2, 3}}, Protocol)
        ),
        ?_assertEqual(
            {6, <<1, 2, 5>>},
            mc_protocol:encode(
                {nested, clientbound},
                #{field0 => [#{nested0 => 2, nested1 => 5}]},
                Protocol
            )
        )
    ].
