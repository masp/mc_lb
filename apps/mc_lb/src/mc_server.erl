-module(mc_server).

-export([connect/3, ping/2]).

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
    Protocol = mc_protocol:init(),
    {ok, Socket} = connect_base(Address, Port, Protocol),
    LoginSocket = handshake(Socket, Protocol, login),
    PlaySocket = login(LoginSocket, Player),
    {ok, PlaySocket}.

-spec ping(Address, Port) -> {ok, Status} | {error, term()} when
    Address :: inet:socket_address() | inet:hostname(),
    Port :: inet:port_number(),
    Status :: #{
        ping => non_neg_integer(),
        max_players => non_neg_integer(),
        online_players => non_neg_integer()
    }.
ping(Address, Port) ->
    Protocol = mc_protocol:init(),
    case connect_base(Address, Port, Protocol) of
        {ok, Socket} ->
            StatusSocket = handshake(Socket, Protocol, status),
            Ret = do_ping(StatusSocket),
            mc_socket:shutdown(StatusSocket),
            Ret;
        {error, Reason} ->
            {error, Reason}
    end.

connect_base(Address, Port, Protocol) ->
    case gen_tcp:connect(Address, Port, [binary, {active, false}]) of
        {ok, TcpSocket} ->
            {ok, mc_socket:wrap(TcpSocket, serverbound, Protocol)};
        {error, Reason} ->
            {error, Reason}
    end.

handshake(Socket, Protocol, NextState) ->
    mc_socket:send(
        Socket,
        handshake,
        #{
            protocol => mc_protocol:version(Protocol),
            address => <<"">>,
            port => 0,
            nextstate => NextState
        }
    ),
    mc_socket:change_state(Socket, NextState).

login(Socket, #{name := Name} = Player) ->
    mc_socket:send(Socket, login_start, #{name => Name}),
    case mc_socket:recv(Socket) of
        {packet, login_success, _Packet} ->
            mc_socket:change_state(Socket, play);
        {packet, _PacketName, _Packet} ->
            % TODO: handle compression and encryption
            login(Socket, Player)
    end.

query_status(Socket) ->
    ok = mc_socket:send(Socket, status_req, #{}),
    {packet, status_resp, #{json_resp := JsonInfo}} = mc_socket:recv(Socket),
    jsone:decode(JsonInfo).

query_ping(Socket) ->
    ok = mc_socket:send(Socket, ping, #{payload => 12345}),
    T1 = erlang:system_time(millisecond),
    {packet, pong, #{payload := 12345}} = mc_socket:recv(Socket),
    T2 = erlang:system_time(millisecond),
    T2 - T1.

do_ping(Socket) ->
    #{
        <<"players">> := #{
            <<"max">> := MaxPlayers,
            <<"online">> := OnlinePlayers
        }
    } = query_status(Socket),
    Ping = query_ping(Socket),
    {ok, #{ping => Ping, max_players => MaxPlayers, online_players => OnlinePlayers}}.
