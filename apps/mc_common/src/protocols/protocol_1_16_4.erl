-module(protocol_1_16_4).
-behaviour(protocol_definition).

-include("protocol_utils.hrl").

-export([version/0, packet/1, lookup_packet_info/1]).

version() -> 754.

lookup_packet_info(PacketInfo) ->
    case PacketInfo of
        ?PACKET(16#00, handshaking, serverbound, handshake);

        ?PACKET(16#00, status, serverbound, status_req);
        ?PACKET(16#01, status, serverbound, ping);

        ?PACKET(16#00, status, clientbound, status_resp);
        ?PACKET(16#01, status, clientbound, pong);

        ?PACKET(16#00, login, serverbound, login_start);
        ?PACKET(16#01, login, serverbound, encryption_response);
    
        ?PACKET(16#00, login, clientbound, disconnect);
        ?PACKET(16#01, login, clientbound, encryption_request);
        ?PACKET(16#02, login, clientbound, login_success);
        ?PACKET(16#03, login, clientbound, set_compression);

        ?PACKET(16#03, play, serverbound, chat_serverbound);
        ?PACKET(16#0B, play, serverbound, plugin_message);
        ?PACKET(16#10, play, serverbound, keep_alive);

        ?PACKET(16#04, play, clientbound, spawn_player);
        ?PACKET(16#0E, play, clientbound, chat_clientbound);
        ?PACKET(16#1F, play, clientbound, keep_alive);
        ?PACKET(16#24, play, clientbound, join_game);
        ?PACKET(16#32, play, clientbound, player_info);
        ?PACKET(16#39, play, clientbound, respawn);
        _ -> {unknown_packet, PacketInfo}
    end.

%%% Handshake Packets
packet(handshake) ->
    [
        {protocol, varint},
        {address, string},
        {port, u16},
        {nextstate,
            {enum, varint, [
                {status, 1},
                {login, 2}
            ]}}
    ];
%%%
%%% Status Packets
packet(status_req) ->
    [];
packet(status_resp) ->
    [{json_resp, string}];
packet(ping) ->
    [{payload, i64}];
packet(pong) ->
    [{payload, i64}];
%%%
%%% Login Packets
packet(login_start) ->
    [{name, string}];
packet(disconnect) ->
    [{reason, string}];
packet(encryption_request) ->
    [
        {server_id, string},
        {public_key, {binary, varint}},
        {verify_token, {binary, varint}}
    ];
packet(encryption_response) ->
    [
        {shared_secret, {binary, varint}},
        {verify_token, {binary, varint}}
    ];
packet(login_success) ->
    [{uuid, uuid}, {name, string}];
packet(set_compression) ->
    [{threshold, varint}];
%%%
%%% Play Packets
packet(keep_alive) ->
    [{id, i64}];
packet(chat_serverbound) ->
    [{message, string}];
packet(client_settings) ->
    [
        {locale, string},
        {view_distance, i8},
        {chat_mode,
            {enum, varint, [
                {enabled, 0},
                {commands_only, 1},
                {hidden, 2}
            ]}},
        {chat_colors, bool},
        {displayed_skin_parts, u8},
        {main_hand,
            {enum, varint, [
                {left, 0},
                {right, 1}
            ]}}
    ];
packet(plugin_message) ->
    [
        {channel, string},
        {data, rest}
    ];
packet(pos) ->
    [
        {x, double},
        {feet_y, double},
        {z, double},
        {on_ground, bool}
    ];
packet(pos_and_rot) ->
    [
        {x, double},
        {feet_y, double},
        {z, double},
        {yaw, float},
        {pitch, float},
        {on_ground, bool}
    ];
packet(rot) ->
    [
        {yaw, float},
        {pitch, float},
        {on_ground, bool}
    ];
packet(update_view_pos) ->
    [
        {chunk_x, varint},
        {chunk_z, varint}
    ];
packet(chunk_data) ->
    [
        {chunk_x, i32},
        {chunk_z, i32},
        {full_chunk, bool},
        {primary_bitmask,
            {bitmask, varint, [
                % This looks stupid, but is a mapping from section number (0-15) to bit (0-15)
                {0, 0},
                {1, 1},
                {2, 2},
                {3, 3},
                {4, 4},
                {5, 5},
                {6, 6},
                {7, 7},
                {8, 8},
                {9, 9},
                {10, 10},
                {11, 11},
                {12, 12},
                {13, 13},
                {14, 14},
                {15, 15}
            ]}},
        {heightmaps, nbt},
        {biomes, {array, varint, varint}},
        {data, {binary, varint}},
        {block_entities, {array, varint, nbt}}
    ];
packet(join_game) ->
    [
        {entity_id, i32},
        {is_hardcore, bool},
        {gamemode, {enum, u8, gamemode_enum()}},
        {prev_gamemode, {enum, i8, [{no_prev, -1} | gamemode_enum()]}},
        {world_names, {array, varint, string}},
        {dimension_codec, nbt},
        {dimension, nbt},
        {world, string},
        {hashed_seed, i64},
        {max_players, varint},
        {view_distance, varint},
        {reduced_debug_info, bool},
        {enable_respawn_screen, bool},
        {is_debug, bool},
        {is_flat, bool}
    ];
packet(player_info) ->
    [
        {info,
            {option,
                {enum, varint, [
                    {add_player, 0},
                    {update_gamemode, 1},
                    {update_latency, 2},
                    {update_display_name, 3},
                    {remove_player, 4}
                ]},
                [
                    {add_player,
                        player_info_body([
                            {name, string},
                            {props,
                                {array, varint,
                                    {packet, [
                                        {name, string},
                                        {value, string},
                                        {signature, {option, string}}
                                    ]}}},
                            {gamemode, {enum, varint, gamemode_enum()}},
                            {ping, varint},
                            {display_name, {option, string}}
                        ])},
                    {update_gamemode,
                        player_info_body([
                            {new_gamemode, {enum, varint, gamemode_enum()}}
                        ])},
                    {update_latency,
                        player_info_body([
                            {new_latency, varint}
                        ])},
                    {update_display_name,
                        player_info_body([
                            {new_name, {option, string}}
                        ])},
                    {remove_player, player_info_body([])}
                ]}}
    ];
packet(pos_and_look) ->
    [
        {x, double},
        {y, double},
        {z, double},
        {yaw, float},
        {pitch, float},
        {flags, u8},
        {teleport_id, varint}
    ];
packet(spawn_position) ->
    [{location, position}];
packet(player_movement) ->
    [{on_ground, bool}];
packet(spawn_player) ->
    [
        {eid, varint},
        {playerUuid, uuid},
        {x, double},
        {y, double},
        {z, double},
        {yaw, angle},
        {pitch, angle}
    ];
packet(chat_clientbound) ->
    [
        {msg, string},
        {position,
            {enum, u8, [
                {chat, 0},
                {system_msg, 1},
                {game_info, 2}
            ]}},
        {sender, uuid}
    ];
packet(disconnect_play) ->
    [{reason, string}];
packet(entity_head_look) ->
    [
        {entity_id, varint},
        {head_yaw, angle}
    ];
packet(respawn) ->
    [
        {dimension, nbt},
        {world_name, string},
        {hashed_seed, i64},
        {gamemode, {enum, u8, gamemode_enum()}},
        {prev_gamemode, {enum, i8, [{no_prev, -1} | gamemode_enum()]}},
        {is_debug, bool},
        {is_flat, bool},
        {copy_metadata, bool}
    ];
packet(entity_position) ->
    [
        {entity_id, varint},
        {delta_x, i16},
        {delta_y, i16},
        {delta_z, i16},
        {on_ground, bool}
    ];
packet(entity_pos_and_rot) ->
    [
        {entity_id, varint},
        {delta_x, i16},
        {delta_y, i16},
        {delta_z, i16},
        {yaw, angle},
        {pitch, angle},
        {on_ground, bool}
    ];
packet(entity_rot) ->
    [
        {entity_id, varint},
        {yaw, angle},
        {pitch, angle},
        {on_ground, bool}
    ];
packet(entity_movement) ->
    [
        {entity_id, varint}
    ];
packet(remove_entities) ->
    [
        {entity_ids, {array, varint, varint}}
    ].

gamemode_enum() ->
    [
        {survival, 0},
        {creative, 1},
        {adventure, 2},
        {spectator, 3}
    ].

player_info_body(Fields) ->
    {array, varint,
        {packet, [
            {uuid, uuid}
            | Fields
        ]}}.
