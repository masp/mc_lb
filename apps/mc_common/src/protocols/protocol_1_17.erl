-module(protocol_1_17).
-behaviour(protocol_definition).

-include("protocol_utils.hrl").

-export([version/0, packet/1, lookup_packet_info/1]).

version() -> 756.

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
        ?PACKET(16#0A, play, serverbound, plugin_message);
        ?PACKET(16#0F, play, serverbound, keep_alive);

        ?PACKET(16#04, play, clientbound, spawn_player);
        ?PACKET(16#0F, play, clientbound, chat_clientbound);
        ?PACKET(16#1A, play, clientbound, disconnect_play);
        ?PACKET(16#21, play, clientbound, keep_alive);
        ?PACKET(16#26, play, clientbound, join_game);
        ?PACKET(16#36, play, clientbound, player_info);
        ?PACKET(16#3D, play, clientbound, respawn);
        _ -> {unknown_packet, PacketInfo}
    end.

packet(Name) -> protocol_1_16_4:packet(Name).
