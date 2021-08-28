-module(mc_pipe).
-behaviour(gen_server).

-export([start_link/2, bind/3]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-type proxy_socket() :: none | mc_socket:socket().

-opaque pipe() :: pid().
-export_type([pipe/0]).

-record(state, {
    player :: mc_player:player(),
    recv = none :: proxy_socket(),
    send = none :: proxy_socket(),
    filters = [] :: [module()]
}).

-spec start_link(Player, Filters) -> {ok, mc_pipe:pipe()} when
    Player :: mc_player:player(),
    Filters :: [module()].
start_link(Player, Filters) ->
    gen_server:start_link(?MODULE, [Player, Filters], []).

-spec bind(pipe(), Side, Socket) -> ok when
    Side :: recv | send,
    Socket :: mc_socket:socket().
bind(Pipe, Side, Socket) ->
    gen_server:call(Pipe, {bind, Side, Socket}).

init([Player, Filters]) ->
    {ok, #state{
        player = Player,
        filters = Filters
    }}.

handle_call({bind, recv, Socket}, _From, State) ->
    ok = mc_socket:recv_passive(Socket),
    {reply, ok, State#state{recv = Socket}};
handle_call({bind, send, Socket}, _From, State) ->
    {reply, ok, State#state{send = Socket}}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(
    {R, {packet, Name, Packet}},
    #state{recv = R, send = S, filters = Fs, player = Player} = State
) when S =/= none ->
    case run_filters(Player, Name, Packet, Fs) of
        forward ->
            ok = mc_socket:send(S, Name, Packet);
        block ->
            block
    end,
    ok = mc_socket:recv_passive(R),
    {noreply, State};
handle_info(
    {R, {unknown_packet, ID, PacketBin}},
    #state{recv = R, send = S} = State
) when S =/= none ->
    ok = mc_socket:send_raw(S, ID, PacketBin),
    ok = mc_socket:recv_passive(R),
    {noreply, State};
handle_info(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal functions
run_filters(_Player, _Name, _Packet, []) ->
    forward;
run_filters(Player, Name, Packet, [F | Fs]) ->
    case mc_pipe_filter:filter(F, Player, Name, Packet) of
        forward -> run_filters(Player, Name, Packet, Fs);
        block -> block
    end.
