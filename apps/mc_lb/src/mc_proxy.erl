-module(mc_proxy).
-behavior(gen_server).

-export([start_link/1]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2
]).

-record(state, {
    client = mc_socket:socket(),
    server = mc_socket:socket()
}).

-spec start_link(ClientSocket :: mc_socket:socket()) -> {ok, pid()}.
start_link(CSocket) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [CSocket], []).

init([CSocket]) ->
    {ok, SSocket} = mc_server:connect("localhost", 25564, #{name => <<"ttt">>}),
    mc_socket:set_active(SSocket),
    mc_socket:set_active(CSocket),
    {ok, #state{
        client = CSocket,
        server = SSocket
    }}.

handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(
    {packet, From, {Name, Packet}},
    #state{client = Client, server = Server} = State
) ->
    case From of
        Client ->
            ok = mc_socket:send(Server, Name, Packet),
            mc_socket:set_active(Client);
        Server ->
            ok = mc_socket:send(Client, Name, Packet),
            mc_socket:set_active(Server)
    end,
    {noreply, State};
handle_info(
    {unknown_packet, From, {ID, PacketBin}},
    #state{client = Client, server = Server} = State
) ->
    case From of
        Client ->
            ok = mc_socket:send_raw(Server, ID, PacketBin),
            mc_socket:set_active(Client);
        Server ->
            ok = mc_socket:send_raw(Client, ID, PacketBin),
            mc_socket:set_active(Server)
    end,
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.
