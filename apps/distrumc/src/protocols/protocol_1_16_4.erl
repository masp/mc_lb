-module(protocol_1_16_4).

-export([packet/1, lookup_id/2, lookup_name/3]).

lookup_id(Name, Dir) -> packet_info(id, {Name, Dir}).

lookup_name(ID, State, Dir) -> packet_info(name, {ID, State, Dir}).

-define(PACKET(ID, State, Dir, Name), 
    {name, {ID, State, Dir}} -> Name;
    {id, {Name, Dir}} -> ID).

packet_info(Type, Args) ->
    case {Type, Args} of
        ?PACKET(16#00, handshaking, serverbound, handshake);

        ?PACKET(16#00, status, serverbound, status_req);
        ?PACKET(16#01, status, serverbound, ping);
        ?PACKET(16#00, status, clientbound, status_resp);
        ?PACKET(16#01, status, clientbound, ping);

        ?PACKET(16#00, login, serverbound, login_start);
        ?PACKET(16#02, login, clientbound, login_success);

        ?PACKET(16#00, play, serverbound, teleport_confirm);
        ?PACKET(16#05, play, serverbound, client_settings);
        ?PACKET(16#0B, play, serverbound, plugin_message);
        ?PACKET(16#10, play, serverbound, keep_alive);
        ?PACKET(16#12, play, serverbound, pos);
        ?PACKET(16#13, play, serverbound, pos_and_rot);
        ?PACKET(16#14, play, serverbound, rot);

        ?PACKET(16#1F, play, clientbound, keep_alive);
        ?PACKET(16#20, play, clientbound, chunk_data);
        ?PACKET(16#24, play, clientbound, join_game);
        ?PACKET(16#34, play, clientbound, pos_and_look);
        ?PACKET(16#40, play, clientbound, update_view_pos);
        ?PACKET(16#42, play, clientbound, spawn_position);
        _ -> {unknown_packet, Args}
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
%%%
%%% Login Packets
packet(login_start) ->
    [{name, string}];
packet(login_success) ->
    [{uuid, uuid}, {name, string}];
packet(keep_alive) ->
    [{id, i64}];
packet(teleport_confirm) ->
    [{teleport_id, varint}];
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
        {gamemode,
            {enum, u8, [
                {survival, 0},
                {creative, 1},
                {adventure, 2},
                {spectator, 3}
            ]}},
        {prev_gamemode,
            {enum, i8, [
                {none, -1},
                {survival, 0},
                {creative, 1},
                {adventure, 2},
                {spectator, 3}
            ]}},
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
    [{location, position}].
