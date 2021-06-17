-module(mc_acceptor).

-export([start_link/1]).

-export([loop/1]).

-spec start_link(gen_tcp:socket()) -> {ok, pid()}.
start_link(LSocket) ->
    Pid = spawn_link(?MODULE, loop, [LSocket]),
    {ok, Pid}.

loop(LSocket) ->
    {ok, Socket} = gen_tcp:accept(LSocket),
    McSocket = mc_socket:wrap(Socket, clientbound, mc_protocol:init()),
    {packet, handshake, #{nextstate := NextState}} = mc_socket:recv(McSocket),
    McSocket2 = mc_socket:change_state(McSocket, NextState),
    ok = handle_client(McSocket2, NextState),
    loop(LSocket).

handle_client(McSocket, login) ->
    PlaySocket = handle_login(McSocket),
    start_child(PlaySocket),
    ok;
handle_client(McSocket, status) ->
    case mc_socket:recv(McSocket) of
        {packet, status_req, _} ->
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
            mc_socket:send(McSocket, status_resp, #{json_resp => JsonResp}),
            handle_client(McSocket, status);
        {packet, ping, Packet} ->
            mc_socket:send(McSocket, pong, Packet),
            handle_client(McSocket, status);
        {error, _} ->
            ok
    end.

handle_login(McSocket) ->
    case mc_socket:recv(McSocket) of
        {packet, login_start, #{name := PlayerName}} ->
            mc_socket:send(McSocket, login_success, #{
                name => PlayerName,
                uuid => uuid:get_v3(<<"OfflinePlayer:">>, PlayerName)
            }),
            PlaySocket = mc_socket:change_state(McSocket, play),
            PlaySocket
        % TODO: Handle encryption and compression
    end.

start_child(McSocket) ->
    % Takes control of the TCP socket in start_link
    {ok, Pid} = supervisor:start_child(mc_players_sup, #{
        id => {mc_proxy, make_ref()},
        start => {mc_proxy, start_link, [McSocket]},
        restart => temporary
    }),
    ok = mc_socket:controlling_process(McSocket, Pid).
