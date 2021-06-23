-module(mc_server).

-export([connect/3]).

-type player() :: #{name => binary()}.

-type info() :: #{
    name => binary(),
    address => inet:socket_address() | inet:hostname(),
    port => inet:port_number(),
    protocol => mc_protocol:protocol(),
    description => binary(),
    max_players => non_neg_integer()
}.

-export_type([info/0]).

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
            mc_socket:change_state(Socket, play);
        {packet, _PacketName, _Packet} ->
            % TODO: handle compression and encryption
            login(Socket, Player)
    end.
