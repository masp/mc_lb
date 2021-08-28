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
    {ok, CPipe} = mc_pipe:start_link(self(), []),
    {ok, SPipe} = mc_pipe:start_link(self(), [
        mc_command_filter
    ]),
    ok = mc_pipe:bind(CPipe, send, CSocket),
    ok = mc_pipe:bind(SPipe, recv, CSocket),
    self() ! {switch_server, default},
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
    do_send_msg(State, Msg),
    {reply, ok, State};
handle_call({switch_server, Name}, _From, State) ->
    % So that the switch happens asynchronously from the gen_server call
    self() ! {switch_server, Name},
    {reply, ok, State};
handle_call({disconnect, Reason}, _From, #state{csock = CSock}) ->
    mc_socket:send(CSock, disconnect, #{reason => Reason}),
    exit(normal).

handle_cast(_Msg, State) ->
    {noreply, State}.

% Handle if any sockets close unexpectedly
handle_info({switch_server, ServerName}, State) ->
    case mc_server_registry:find_server(ServerName) of
        {ok, {Address, Port}} ->
            NewState = connect_new_server(State, Address, Port),
            disconnect_old_server(State),
            {noreply, NewState};
        server_not_found ->
            do_send_msg(State, ["World with name '", ServerName, "' does not exist"]),
            {noreply, State}
    end.

terminate(_Reason, #state{csock = CSock, ssock = SSock}) ->
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
        msg => mc_chat:info(Msg),
        position => system_msg,
        sender => <<0:128>>
    }).

connect_new_server(#state{cpipe = CPipe, spipe = SPipe} = State, Address, Port) ->
    {ok, NewSSocket} = mc_server:connect(Address, Port, #{name => <<"ttt">>}),
    ok = mc_pipe:bind(CPipe, recv, NewSSocket),
    ok = mc_pipe:bind(SPipe, send, NewSSocket),
    State#state{ssock = NewSSocket}.

disconnect_old_server(#state{ssock = none}) ->
    ok;
disconnect_old_server(#state{ssock = Socket}) ->
    mc_socket:shutdown(Socket).
