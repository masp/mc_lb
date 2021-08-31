-module(mc_socket).
-behaviour(gen_server).

% API
-export([
    connect/4,
    wrap/3, wrap/4,
    send/3,
    send_raw/3,
    recv/1,
    recv_passive/1,
    change_state/2,
    set_encryption/2,
    shutdown/1
]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2
]).

% mc_socket abstracts away communication using the Minecraft protocol
%
% For example, to talk to a client
%
%   Socket = mc_socket:init(TcpSocket, clientbound, #{}),
%   ok = mc_socket:send(Socket, handshake, #{}),
%   {packet, login_success, LoginSuccess} = mc_socket:recv(Socket)
%

-record(sock, {
    tcp_socket :: gen_tcp:socket() | closed,
    protocol :: mc_protocol:protocol(),
    dir = clientbound :: mc_protocol:packet_dir(),
    timeout = 30000 :: non_neg_integer(),
    mc_state = handshaking :: mc_protocol:packet_state(),
    buffer = <<>> :: binary(),
    encrypt = none :: none | crypto:crypto_state(),
    decrypt = none :: none | crypto:crypto_state(),
    requester = none :: none | pid()
}).

-type socket() :: pid().
-export_type([socket/0]).

-spec connect(Address, Port, ToDir, Protocol) -> {ok, socket()} when
    Address :: inet:socket_address() | inet:hostname(),
    Port :: inet:port_number(),
    ToDir :: mc_protocol:packet_dir(),
    Protocol :: mc_protocol:protocol().
connect(Address, Port, ToDir, Protocol) ->
    {ok, TcpSock} = gen_tcp:connect(Address, Port, [binary, {active, false}]),
    wrap(TcpSock, ToDir, Protocol).

-spec wrap(TcpSock, ToDir, Protocol) -> {ok, socket()} when
    TcpSock :: gen_tcp:socket(),
    ToDir :: mc_protocol:packet_dir(),
    Protocol :: mc_protocol:protocol().
wrap(TcpSock, ToDir, Protocol) ->
    wrap(TcpSock, ToDir, Protocol, #{}).

-spec wrap(TcpSock, ToDir, Protocol, Opts) -> {ok, socket()} when
    TcpSock :: gen_tcp:socket(),
    ToDir :: mc_protocol:packet_dir(),
    Protocol :: mc_protocol:protocol(),
    Opts :: #{timeout => non_neg_integer(), state => mc_protocol:packet_state()}.
wrap(TcpSock, ToDir, Protocol, Opts) ->
    McSocket = #sock{
        tcp_socket = TcpSock,
        protocol = Protocol,
        dir = ToDir,
        mc_state = maps:get(state, Opts, handshaking),
        timeout = maps:get(timeout, Opts, 30000)
    },
    {ok, Pid} = gen_server:start_link(?MODULE, [McSocket], []),
    gen_tcp:controlling_process(TcpSock, Pid),
    {ok, Pid}.

init([McSocket]) ->
    {ok, McSocket}.

-spec recv(socket()) -> RecvPacket | {error, timeout} | closed when
    RecvPacket :: {packet, Name, Packet} | {unknown_packet, ID, PacketBin},
    ID :: mc_protocol:packet_id(),
    Name :: mc_protocol:packet_name(),
    Packet :: map(),
    PacketBin :: binary().
recv(Socket) ->
    ok = gen_server:call(Socket, recv),
    receive
        {Socket, Response} -> Response
    after 30000 ->
        % Timeout after 30 seconds
        {error, timeout}
    end.

-spec recv_passive(socket()) -> ok | closed.
recv_passive(Socket) ->
    % Non-blocking receive
    gen_server:call(Socket, recv).

%% send sends a Minecraft packet over the TCP connection
%%
%% Example:
%%   ok = mc_socket:send(Socket, {login_success, })
-spec send(socket(), Name, Packet) -> ok | closed | {error, Reason} when
    Name :: mc_protocol:packet_name(),
    Packet :: map(),
    Reason :: closed | {timeout, binary()} | inet:posix().
send(Socket, Name, Packet) ->
    gen_server:call(Socket, {send, Name, Packet}).

-spec send_raw(socket(), ID, PacketBin) -> ok | closed | {error, Reason} when
    ID :: mc_protocol:packet_id(),
    PacketBin :: binary(),
    Reason :: closed | {timeout, binary()} | inet:posix().
send_raw(Socket, ID, PacketBin) ->
    gen_server:call(Socket, {send_raw, ID, PacketBin}).

-spec change_state(socket(), NewState :: mc_protocol:packet_state()) -> closed | ok.
change_state(Socket, NewState) ->
    gen_server:call(Socket, {change_state, NewState}).

-spec set_encryption(socket(), <<_:128>>) -> closed | ok.
set_encryption(Socket, SharedCode) ->
    gen_server:call(Socket, {set_encryption, SharedCode}).

-spec shutdown(socket()) -> ok.
shutdown(Socket) ->
    try
        gen_server:stop(Socket)
    catch
        % Don't care if the process is already terminated (socket closed)
        _:noproc -> ok
    end.

handle_call(_Msg, _From, #sock{tcp_socket = closed, buffer = <<>>} = Socket) ->
    {reply, closed, Socket};
handle_call(recv, {From, _Ref}, #sock{requester = Requester} = Socket) ->
    case Requester of
        none ->
            % In case we have a full packet already buffered, try decoding
            {reply, ok, do_recv(Socket#sock{requester = From})};
        From ->
            {reply, ok, Socket};
        _ ->
            {reply, socket_already_used, Socket}
    end;
handle_call({change_state, NewState}, _From, Socket) ->
    {reply, ok, Socket#sock{mc_state = NewState}};
handle_call({set_encryption, none}, _From, Socket) ->
    {reply, ok, Socket#sock{encrypt = none, decrypt = none}};
handle_call({set_encryption, SharedCode}, _From, Socket) ->
    Encrypt = crypto:crypto_init(aes_cfb8, SharedCode, SharedCode, [{encrypt, true}]),
    Decrypt = crypto:crypto_init(aes_cfb8, SharedCode, SharedCode, [{encrypt, false}]),
    {reply, ok, Socket#sock{encrypt = Encrypt, decrypt = Decrypt}};
handle_call({send, Name, Packet}, _From, #sock{dir = Dir, protocol = Protocol} = Socket) ->
    {ID, PacketBin} = mc_protocol:encode({Name, Dir}, Packet, Protocol),
    {reply, do_send(Socket, ID, PacketBin), Socket};
handle_call({send_raw, ID, PacketBin}, _From, Socket) ->
    {reply, do_send(Socket, ID, PacketBin), Socket}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({tcp, TcpSock, RecvBytes}, #sock{tcp_socket = TcpSock, requester = none} = Socket) ->
    {noreply, update_buffer(Socket, RecvBytes)};
handle_info({tcp, TcpSock, RecvBytes}, #sock{tcp_socket = TcpSock} = Socket) ->
    {noreply, do_recv(update_buffer(Socket, RecvBytes))};
handle_info({tcp_closed, TcpSock}, #sock{tcp_socket = TcpSock, requester = Requester} = Socket) ->
    case Requester of
        none -> ok;
        _ -> Requester ! {self(), closed}
    end,
    {noreply, Socket#sock{tcp_socket = closed}};
handle_info({tcp_error, TcpSock, Reason}, #sock{tcp_socket = TcpSock} = Socket) ->
    {stop, {tcp_error, Reason}, Socket};
handle_info(_Info, Socket) ->
    {noreply, Socket}.

terminate(_Reason, #sock{tcp_socket = closed}) ->
    ok;
terminate(_Reason, #sock{tcp_socket = TcpSocket}) ->
    gen_tcp:shutdown(TcpSocket, read_write),
    ok.

%%% Internal functions

do_send(#sock{tcp_socket = TcpSock} = Socket, ID, PacketBin) ->
    IDBin = mc_varint:encode(ID),
    Len = mc_varint:encode(byte_size(IDBin) + byte_size(PacketBin)),
    EncBin = encrypt_packet(Socket, <<Len/binary, IDBin/binary, PacketBin/binary>>),
    gen_tcp:send(TcpSock, EncBin).

encrypt_packet(#sock{encrypt = none}, Bin) -> Bin;
encrypt_packet(#sock{encrypt = Encrypt}, Bin) -> crypto:crypto_update(Encrypt, Bin).

update_buffer(#sock{buffer = Buffer} = Socket, RecvBytes) ->
    DecBytes = decrypt_packet(Socket, RecvBytes),
    Socket#sock{buffer = <<Buffer/binary, DecBytes/binary>>}.

decrypt_packet(#sock{decrypt = none}, Bin) -> Bin;
decrypt_packet(#sock{decrypt = Decrypt}, Bin) -> crypto:crypto_update(Decrypt, Bin).

-spec do_recv(#sock{}) -> #sock{}.
do_recv(#sock{buffer = Buffer, requester = Requester} = Socket) ->
    case decode_packet(Socket, Buffer) of
        {Result, Remaining} ->
            Requester ! {self(), Result},
            Socket#sock{buffer = Remaining, requester = none};
        needmore ->
            % Receive more data to parse the rest of the packet
            recv_more(Socket),
            Socket
    end.

-spec decode_packet(Socket, Buffer) ->
    needmore
    | {{packet, Name, Packet}, Remaining}
    | {{unknown_packet, ID, binary()}, Remaining}
when
    Socket :: #sock{},
    Buffer :: binary(),
    Name :: mc_protocol:packet_name(),
    Packet :: mc_protocol:packet(),
    ID :: mc_protocol:packet_id(),
    Remaining :: binary().
decode_packet(Socket, Buffer) ->
    case mc_varint:decode(Buffer) of
        {Len, Body} ->
            if
                byte_size(Body) >= Len ->
                    <<Packet:Len/binary-unit:8, Remaining/binary>> = Body,
                    {decode_packet_body(Socket, Packet), Remaining};
                true ->
                    needmore
            end;
        needmore ->
            needmore
    end.

inverse_dir(clientbound) -> serverbound;
inverse_dir(serverbound) -> clientbound.

decode_packet_body(#sock{protocol = Protocol, dir = Dir, mc_state = State}, PacketBin) ->
    {ID, Body} = mc_varint:decode(PacketBin),
    case mc_protocol:decode_id(ID, State, inverse_dir(Dir), Protocol) of
        {unknown_packet, _} ->
            {unknown_packet, ID, Body};
        Name ->
            Packet = mc_protocol:decode(Name, Body, Protocol),
            {packet, Name, Packet}
    end.

recv_more(#sock{tcp_socket = closed}) ->
    ok;
recv_more(#sock{tcp_socket = TcpSock}) ->
    inet:setopts(TcpSock, [binary, {active, once}]).
