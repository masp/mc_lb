-module(mc_socket).

% API
-export([
    wrap/3, wrap/4,
    send/3,
    send_raw/3,
    recv/1,
    change_state/2,
    set_active/1,
    controlling_process/2,
    shutdown/1
]).

% mc_socket abstracts away communication using the Minecraft protocol
%
% For example, to talk to a client
%
%   Socket = mc_socket:init(TcpSocket, clientbound, #{}),
%   ok = mc_socket:send(Socket, handshake, #{}),
%   {Socket2, {packet, login_success, LoginSuccess}} = mc_socket:recv(Socket)
%

-record(sock, {
    mc_socket_pid = self() :: pid(),
    tcp_socket :: gen_tcp:socket(),
    protocol :: mc_protocol:protocol(),
    dir = clientbound :: mc_protocol:packet_dir(),
    timeout = 30000 :: non_neg_integer(),
    mc_state = handshaking :: mc_protocol:packet_state()
}).

-opaque socket() :: #sock{}.
-export_type([socket/0]).

-spec wrap(TcpSock, ToDir, Protocol) -> socket() when
    TcpSock :: gen_tcp:socket(),
    ToDir :: mc_protocol:packet_dir(),
    Protocol :: mc_protocol:protocol().
wrap(TcpSock, ToDir, Protocol) ->
    wrap(TcpSock, ToDir, Protocol, #{}).

-spec wrap(TcpSock, ToDir, Protocol, Opts) -> socket() when
    TcpSock :: gen_tcp:socket(),
    ToDir :: mc_protocol:packet_dir(),
    Protocol :: mc_protocol:protocol(),
    Opts :: #{timeout => non_neg_integer()}.
wrap(TcpSock, ToDir, Protocol, Opts) ->
    McSocket = #sock{
        tcp_socket = TcpSock,
        protocol = Protocol,
        dir = ToDir,
        timeout = maps:get(timeout, Opts, 30000)
    },
    McSocketPid = spawn_link(fun() -> start_loop(McSocket#sock{mc_socket_pid = self()}) end),
    McSocket#sock{mc_socket_pid = McSocketPid}.

-spec recv(socket()) -> RecvPacket when
    RecvPacket :: {packet, Name, Packet} | {error, Reason} | {unknown_packet, ID, PacketBin},
    ID :: mc_protocol:packet_id(),
    Name :: mc_protocol:packet_name(),
    Packet :: map(),
    PacketBin :: binary(),
    Reason :: closed | {timeout, binary()}.
recv(#sock{mc_socket_pid = Pid} = S) ->
    Pid ! {recv, self()},
    receive
        {packet, S, {Name, Packet}} ->
            {packet, Name, Packet};
        {unknown_packet, S, {ID, PacketBin}} ->
            {unknown_packet, ID, PacketBin};
        {mc_error, S, Reason} ->
            {error, Reason}
    end.

%% send sends a Minecraft packet over the TCP connection
%%
%% Example:
%%   ok = mc_socket:send(Socket, {login_success, })
-spec send(socket(), Name, Packet) -> ok | {error, Reason} when
    Name :: mc_protocol:packet_name(),
    Packet :: map(),
    Reason :: closed | {timeout, binary()} | inet:posix().
send(#sock{protocol = Protocol, dir = Dir} = Socket, Name, Packet) ->
    {ID, Body} = mc_protocol:encode({Name, Dir}, Packet, Protocol),
    send_raw(Socket, ID, Body).

-spec send_raw(socket(), ID, PacketBin) -> ok | {error, Reason} when
    ID :: mc_protocol:packet_id(),
    PacketBin :: binary(),
    Reason :: closed | {timeout, binary()} | inet:posix().
send_raw(#sock{tcp_socket = TcpSock}, ID, PacketBin) ->
    IDBin = mc_varint:encode(ID),
    Len = mc_varint:encode(byte_size(IDBin) + byte_size(PacketBin)),
    gen_tcp:send(TcpSock, <<Len/binary, IDBin/binary, PacketBin/binary>>).

-spec change_state(socket(), NewState :: mc_protocol:packet_state()) -> socket().
change_state(#sock{mc_socket_pid = Pid} = Socket, NewState) ->
    Pid ! {change_state, self(), NewState},
    receive
        {state_changed, Socket} ->
            Socket#sock{mc_state = NewState}
    end.

-spec set_active(socket()) -> ok.
set_active(#sock{mc_socket_pid = Pid}) ->
    Pid ! {recv, self()},
    ok.

-spec controlling_process(socket(), NewOwner :: pid()) -> ok | {error, Reason :: term()}.
controlling_process(#sock{tcp_socket = TcpSock}, NewOwner) ->
    gen_tcp:controlling_process(TcpSock, NewOwner).

-spec shutdown(socket()) -> ok.
shutdown(#sock{mc_socket_pid = Pid, tcp_socket = TcpSocket}) ->
    unlink(Pid),
    exit(Pid, kill),
    gen_tcp:shutdown(TcpSocket, read_write).

start_loop(Socket) ->
    loop(Socket, <<>>).

loop(Socket, Buffer) ->
    receive
        {recv, From} ->
            case loop_decode_packet(Socket, Buffer) of
                {{packet, Name, Packet}, Remaining} ->
                    From ! {packet, Socket, {Name, Packet}},
                    loop(Socket, Remaining);
                {{unknown_packet, ID, PacketBin}, Remaining} ->
                    From ! {unknown_packet, Socket, {ID, PacketBin}},
                    loop(Socket, Remaining);
                {error, Reason} ->
                    From ! {mc_error, Socket, Reason},
                    exit(normal)
            end;
        {change_state, From, NewState} ->
            From ! {state_changed, Socket},
            loop(Socket#sock{mc_state = NewState}, Buffer)
    end.

loop_decode_packet(Socket, Buffer) ->
    case mc_varint:decode(Buffer) of
        {Len, Body} ->
            if
                byte_size(Body) >= Len ->
                    <<Packet:Len/binary-unit:8, Remaining/binary>> = Body,
                    {decode_packet(Socket, Packet), Remaining};
                true ->
                    do_recv(Socket, Buffer)
            end;
        needmore ->
            do_recv(Socket, Buffer)
    end.

do_recv(#sock{tcp_socket = TcpSock, timeout = Timeout} = Socket, Buffer) ->
    case gen_tcp:recv(TcpSock, 0, Timeout) of
        {ok, Bin} ->
            loop_decode_packet(Socket, <<Buffer/binary, Bin/binary>>);
        {error, Reason} ->
            {error, Reason}
    end.

inverse_dir(clientbound) -> serverbound;
inverse_dir(serverbound) -> clientbound.

decode_packet(#sock{protocol = Protocol, dir = Dir, mc_state = State}, PacketBin) ->
    {ID, Body} = mc_varint:decode(PacketBin),
    case mc_protocol:decode_id(ID, State, inverse_dir(Dir), Protocol) of
        {unknown_packet, _} ->
            {unknown_packet, ID, Body};
        Name ->
            Packet = mc_protocol:decode(Name, Body, Protocol),
            {packet, Name, Packet}
    end.
