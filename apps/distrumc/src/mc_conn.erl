-module(mc_conn).

-behavior(gen_statem).

-include("mc.hrl").

-export([start_link/1, receive_packet/3]).
-export([init/1, callback_mode/0, handshaking/3, status/3, login/3, play/3]).

-define(TIMEOUT, 20000).
-define(DECODE_PACKET(ID, Body, Protocol),
    decode_packet({ID, ?FUNCTION_NAME, serverbound}, Body, Protocol)
).

start_link(Socket) ->
    gen_statem:start_link(?MODULE, [Socket], []).

callback_mode() ->
    [state_functions, state_enter].

init([Socket]) ->
    process_flag(trap_exit, true),
    mc_conn_reader:start_link(self(), Socket),
    {ok, handshaking, #{
        socket => Socket,
        protocol => mc_protocol:init()
    }}.

receive_packet(Pid, ID, Body) ->
    ok = gen_statem:call(Pid, {recvpacket, {ID, Body}}).

decode_packet({ID, State, Dir}, Body, Protocol) ->
    Name = mc_protocol:lookup_packet_name(ID, State, Dir, Protocol),
    {Name, mc_protocol:decode_packet(Name, Body, Protocol)}.

send_packet(Name, Packet, #{socket := Socket, protocol := Protocol}) ->
    {ID, Body} = mc_protocol:encode_packet({clientbound, Name}, Packet, Protocol),
    IDBin = mc_varint:encode(ID),
    Len = mc_varint:encode(byte_size(IDBin) + byte_size(Body)),
    case gen_tcp:send(Socket, <<Len/binary, IDBin/binary, Body/binary>>) of
        ok ->
            ok;
        {error, closed} ->
            exit(normal);
        {error, Error} ->
            exit(Error)
    end.

handshaking(enter, _Event, _Data) ->
    keep_state_and_data;
handshaking(info, {'EXIT', _From, Reason}, _Data) ->
    handle_exit(Reason);
handshaking({call, From}, {recvpacket, {ID, Body}}, #{protocol := Protocol} = Data) ->
    {handshake, #{nextstate := NextState}} = ?DECODE_PACKET(ID, Body, Protocol),
    {next_state, NextState, Data, [{reply, From, ok}]}.

status(enter, _OldState, _Data) ->
    io:format("entering status~n"),
    keep_state_and_data;
status(info, {'EXIT', _From, Reason}, _Data) ->
    handle_exit(Reason);
status({call, From}, {recvpacket, {ID, Body}}, #{protocol := Protocol} = Data) ->
    {Name, Packet} = ?DECODE_PACKET(ID, Body, Protocol),
    io:format("[status] ~p: ~p~n", [Name, Packet]),
    case Name of
        status_req ->
            JsonResp = jsone:encode(#{
                version => #{
                    name => <<"1.16.4">>,
                    protocol => 754
                },
                players => #{
                    max => 100,
                    % TODO: Get centralized count of players from somewhere
                    online => 0,
                    sample => []
                },
                description => #{
                    text => <<"It's a server?">>
                }
            }),
            send_packet(status_resp, #{json_resp => JsonResp}, Data);
        ping ->
            send_packet(ping, Packet, Data)
    end,
    {keep_state, Data, [{reply, From, ok}]}.

login(enter, _OldState, _Data) ->
    io:format("entering login~n"),
    keep_state_and_data;
login(info, {'EXIT', _From, Reason}, _Data) ->
    handle_exit(Reason);
login({call, From}, {recvpacket, {ID, Body}}, #{protocol := Protocol} = Data) ->
    {Name, Packet} = ?DECODE_PACKET(ID, Body, Protocol),
    io:format("[login] ~p: ~p~n", [Name, Packet]),
    case Name of
        login_start ->
            #{name := PlayerName} = Packet,
            {ok, Uuid} = mc_server:join(PlayerName),
            send_packet(login_success, #{name => PlayerName, uuid => Uuid}, Data),
            {next_state, play, Data#{name => PlayerName, player_uuid => Uuid}, [{reply, From, ok}]}
    end.

send_keep_alive(Data) ->
    Now = erlang:monotonic_time(second),
    send_packet(keep_alive, #{id => Now}, Data),
    Data#{keep_alive_tm => Now}.

play(enter, _OldState, #{protocol := Protocol, player_uuid := Uuid, name := PlayerName} = Data) ->
    io:format("entering play~n"),
    {ok, _} = timer:send_interval(10000, keep_alive),
    send_packet(
        join_game,
        #{
            entity_id => 123,
            is_hardcore => false,
            gamemode => creative,
            prev_gamemode => none,
            world_names => [<<"minecraft:overworld">>],
            dimension_codec => mc_registry:default_dimension_codec(),
            dimension => mc_registry:default_dimension(),
            world => <<"minecraft:overworld">>,
            hashed_seed => 1234567,
            max_players => 0,
            view_distance => 10,
            reduced_debug_info => false,
            enable_respawn_screen => false,
            is_debug => false,
            is_flat => false
        },
        Data
    ),
    [
        begin
            Chunk = mc_chunk:generate_chunk_column({X, Z}),
            send_packet(chunk_data, mc_chunk:encode_chunk_column(Chunk, Protocol), Data)
        end
        || X <- lists:seq(-4, 4), Z <- lists:seq(-4, 4)
    ],
    send_packet(
        player_info,
        #{
            info =>
                {add_player, [
                    #{
                        uuid => Uuid,
                        name => PlayerName,
                        props => [],
                        gamemode => creative,
                        ping => 75
                    }
                ]}
        },
        Data
    ),
    send_packet(
        player_info,
        #{
            info =>
                {update_latency, [
                    #{
                        uuid => Uuid,
                        new_latency => 75
                    }
                ]}
        },
        Data
    ),
    send_packet(
        pos_and_look,
        #{x => 0, y => 64, z => 0, yaw => 0, pitch => 0, flags => 0, teleport_id => 0},
        Data
    ),
    send_packet(update_view_pos, #{chunk_x => 0, chunk_z => 0}, Data),
    send_packet(spawn_position, #{location => {0, 0, 0}}, Data),
    {ok, ExistingPlayers} = mc_server:spawn(),
    [spawn_player(P, Data) || P <- ExistingPlayers],
    {keep_state, send_keep_alive(Data)};
play(info, {'EXIT', _From, Reason}, _Data) ->
    handle_exit(Reason);
play(info, keep_alive, Data) ->
    case Data of
        #{keep_alive_tm := 0} ->
            {keep_state, send_keep_alive(Data)};
        #{keep_alive_tm := Time} ->
            Now = erlang:monotonic_time(second),
            if
                Now - Time >= ?TIMEOUT ->
                    error(timeout);
                true ->
                    keep_state_and_data
            end
    end;
play({call, From}, {recvpacket, {ID, Body}}, #{protocol := Protocol} = Data) ->
    {Name, Packet} = ?DECODE_PACKET(ID, Body, Protocol),
    case Name of
        keep_alive ->
            #{id := RecvTime} = Packet,
            case Data of
                #{keep_alive_tm := RecvTime} ->
                    {keep_state, Data#{keep_alive_tm := 0}, [{reply, From, ok}]};
                #{keep_alive_tm := Time} ->
                    io:format("received mismatched keep alive packet (expected ~p, got ~p)~n", [
                        Time,
                        RecvTime
                    ]),
                    {keep_state, Data, [{reply, From, ok}]}
            end;
        pos_and_rot ->
            {keep_state, maps:merge(Data, Packet), [{reply, From, ok}]};
        pos ->
            {keep_state, maps:merge(Data, Packet), [{reply, From, ok}]};
        rot ->
            {keep_state, maps:merge(Data, Packet), [{reply, From, ok}]};
        _ ->
            io:format("[play] unhandled packet ~p~n", [Name]),
            {keep_state, Data, [{reply, From, ok}]}
    end;
play({call, From}, {spawn_player, Player}, Data) ->
    spawn_player(Player, Data),
    {keep_state, Data, [{reply, From, ok}]};
play({call, From}, {remove_player, #player{eid = Eid}}, Data) ->
    send_packet(remove_entities, #{entity_ids => [Eid]}, Data),
    {keep_state, Data, [{reply, From, ok}]}.

spawn_player(
    #player{
        eid = Eid,
        uuid = Uuid,
        name = Name,
        pos = {X, Y, Z},
        rot = {Yaw, Pitch}
    },
    #{name := MyName} = Data
) ->
    io:format("spawning player ~p for ~p~n", [Name, MyName]),
    send_packet(
        player_info,
        #{
            info =>
                {add_player, [
                    #{
                        uuid => Uuid,
                        name => Name,
                        props => [],
                        gamemode => creative,
                        ping => 75
                    }
                ]}
        },
        Data
    ),
    send_packet(
        spawn_player,
        #{
            eid => Eid,
            playerUuid => Uuid,
            x => X,
            y => Y,
            z => Z,
            yaw => Yaw,
            pitch => Pitch
        },
        Data
    ).

handle_exit(disconnected) ->
    % no need to alert supervisor if client just disconnected
    exit(normal);
handle_exit(Reason) ->
    exit(Reason).
