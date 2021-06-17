-module(mc_server).

-export([connect/3]).

-type player() :: #{name => binary()}.

-spec connect(Address, Port, Player) -> {ok, mc_socket:socket()} when
    Address :: inet:socket_address() | inet:hostname(),
    Port :: inet:port_number(),
    Player :: player().
connect(Address, Port, Player) ->
    {ok, TcpSocket} = gen_tcp:connect(Address, Port, [binary, {active, false}]),
    Protocol = mc_protocol:init(),
    Socket = mc_socket:wrap(TcpSocket, serverbound, Protocol),
    LoginSocket = handshake(Socket, Protocol),
    PlaySocket = login(LoginSocket, Player),
    {ok, PlaySocket}.

handshake(Socket, Protocol) ->
    mc_socket:send(
        Socket,
        handshake,
        #{
            protocol => mc_protocol:version(Protocol),
            address => <<"">>,
            port => 0,
            nextstate => login
        }
    ),
    mc_socket:change_state(Socket, login).

login(Socket, #{name := Name} = Player) ->
    mc_socket:send(Socket, login_start, #{name => Name}),
    case mc_socket:recv(Socket) of
        {packet, login_success, _Packet} ->
            io:format("Entering play~n"),
            mc_socket:change_state(Socket, play);
        {packet, PacketName, Packet} ->
            % TODO: handle compression and encryption
            io:format("Ignoring packet ~p: ~p~n", [PacketName, Packet]),
            login(Socket, Player)
    end.
