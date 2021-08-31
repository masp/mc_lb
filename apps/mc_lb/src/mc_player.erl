-module(mc_player).
-behavior(gen_server).

-include_lib("kernel/include/logger.hrl").

-export([start_link/1, send_msg/2, switch_server/2, disconnect/2]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2
]).

-record(state, {
    % server -> client pipe
    cpipe :: mc_pipe:pipe(),
    csock :: mc_socket:socket(),
    % client -> server pipe
    spipe :: mc_pipe:pipe(),
    ssock = none :: none | mc_socket:socket()
}).

-opaque mc_player() :: pid().
-export_type([mc_player/0]).

-spec start_link(ClientSocket) -> {ok, mc_player()} when
    ClientSocket :: mc_socket:socket().
start_link(CSocket) ->
    gen_server:start_link(?MODULE, [CSocket], []).

init([CSocket]) ->
    link(CSocket),
    {ok, CPipe} = mc_pipe:start_link(),
    {ok, SPipe} = mc_pipe:start_link(#{
        chat_serverbound => self()
    }),
    ok = mc_pipe:bind(CPipe, send, CSocket),
    ok = mc_pipe:bind(SPipe, recv, CSocket),
    start_server_switch(default),
    {ok, #state{
        cpipe = CPipe,
        csock = CSocket,
        spipe = SPipe,
        ssock = none
    }}.

-spec send_msg(pid(), Msg) -> ok | timeout when
    Msg :: iodata().
send_msg(Player, Msg) ->
    gen_server:call(Player, {send_msg, Msg}).

-spec switch_server(pid(), NewServer) -> ok when
    NewServer :: binary().
switch_server(Player, NewServer) ->
    gen_server:call(Player, {switch_server, NewServer}).

-spec disconnect(pid(), Reason) -> ok when
    Reason :: binary().
disconnect(Player, Reason) ->
    gen_server:call(Player, {disconnect, Reason}).

handle_call({send_msg, Msg}, _From, State) ->
    do_send_msg(State, mc_chat:info(Msg)),
    {reply, ok, State};
handle_call({switch_server, Name}, _From, State) ->
    start_server_switch(Name),
    {reply, ok, State};
handle_call({disconnect, Reason}, _From, #state{csock = CSock}) ->
    mc_socket:send(CSock, disconnect, #{reason => Reason}),
    exit(normal).

handle_cast(_Msg, State) ->
    {noreply, State}.

% Handle if any sockets close unexpectedly
handle_info({switch_server, ServerName}, #state{ssock = S} = State) ->
    case do_server_switch(ServerName, State) of
        {ok, NewState} -> {noreply, NewState};
        {error, Reason} ->
            case S of
                % If there's no active server to fallback to, we abort. 
                none -> {stop, Reason, State};
                _ -> 
                    do_send_msg(State, Reason),
                    {noreply, State} 
            end
    end;
handle_info(
    {filtered_packet, _Pipe, chat_serverbound, #{message := Msg}},
    #state{ssock = SSock} = State
) ->
    case mc_commands:parse_command(Msg) of
        not_command ->
            mc_socket:send(SSock, chat_serverbound, #{message => Msg}),
            {noreply, State};
        Command ->
            case handle_command(Command, State) of
                unknown_command ->
                    ?LOG_INFO(#{event => unknown_command, command => Msg}),
                    mc_socket:send(SSock, chat_serverbound, #{message => Msg}),
                    {noreply, State};
                ok ->
                    ?LOG_INFO(#{event => executed_command, command => Command}),
                    {noreply, State}
            end
    end.

terminate(Reason, #state{csock = CSock, ssock = SSock}) ->
    mc_socket:send(CSock, disconnect_play, #{reason => Reason}),
    mc_socket:shutdown(CSock),
    case SSock of
        none ->
            ok;
        _ ->
            mc_socket:shutdown(SSock),
            ok
    end.

% Internal functions
do_send_msg(#state{csock = C}, Msg) ->
    mc_socket:send(C, chat_clientbound, #{
        msg => Msg,
        position => system_msg,
        sender => <<0:128>>
    }).

start_server_switch(ServerName) ->
    % There's no guarantee that switch will succeed in a timely manner so we do it asynchronously.
    % So that the switch happens asynchronously from the gen_server call to avoid any deadlocks.
    self() ! {switch_server, ServerName}.

do_server_switch(ToServer, State) ->
    case mc_server_registry:find_server(ToServer) of
        {ok, {Address, Port}} ->
            OldSSock = State#state.ssock,
            NewState = connect_new_server(State, Address, Port),
            disconnect_old_server(OldSSock),
            {ok, NewState};
        {server_offline, Name} ->
            {error, mc_chat:warn(["Server with name '", Name, "' is offline"])};
        server_not_found ->
            {error, mc_chat:warn(["Server with name '", ToServer, "' does not exist"])}
    end.

connect_new_server(#state{ssock = none} = State, Address, Port) ->
    connect_first_server(State, Address, Port);
connect_new_server(#state{ssock = _S} = State, Address, Port) ->
    connect_later_server(State, Address, Port).

connect_first_server(#state{cpipe = CPipe, spipe = SPipe} = State, Address, Port) ->
    {ok, NewSSocket} = mc_server:connect(Address, Port, #{name => <<"ttt">>}),
    ok = mc_pipe:bind(CPipe, recv, NewSSocket),
    ok = mc_pipe:bind(SPipe, send, NewSSocket),
    State#state{ssock = NewSSocket}.

connect_later_server(#state{csock = C, cpipe = CPipe, spipe = SPipe} = State, Address, Port) ->
    {ok, NewSSocket} = mc_server:connect(Address, Port, #{name => <<"ttt">>}),
    {packet, join_game, JoinGame} = mc_socket:recv(NewSSocket),
    mc_socket:send(C, respawn, #{
        dimension => maps:get(dimension, JoinGame),
        world_name => lists:nth(1, maps:get(world_names, JoinGame)),
        hashed_seed => maps:get(hashed_seed, JoinGame),
        gamemode => maps:get(gamemode, JoinGame),
        prev_gamemode => maps:get(prev_gamemode, JoinGame),
        is_debug => maps:get(is_debug, JoinGame),
        is_flat => maps:get(is_flat, JoinGame),
        copy_metadata => true
    }),
    ok = mc_pipe:bind(CPipe, recv, NewSSocket),
    ok = mc_pipe:bind(SPipe, send, NewSSocket),
    State#state{ssock = NewSSocket}.

disconnect_old_server(none) ->
    ok;
disconnect_old_server(Socket) ->
    mc_socket:shutdown(Socket).

handle_command([<<"worlds">>, <<"list">>], State) ->
    do_send_msg(State, mc_chat:info(<<"World list:">>)),
    [do_send_msg(State, mc_chat:info(["- ", S])) || S <- mc_server_registry:list_servers()],
    ok;
handle_command([<<"worlds">>, <<"go">>, Name], State) ->
    do_send_msg(State, mc_chat:info(["Switching to server ", Name])),
    start_server_switch(Name),
    ok;
handle_command(_Args, _Proxy) ->
    unknown_command.
