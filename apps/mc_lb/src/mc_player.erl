-module(mc_player).
-behavior(gen_server).

-include_lib("kernel/include/logger.hrl").

-export([start_link/1, send_msg/2]).

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
    gen_server:start_link({local, ?MODULE}, ?MODULE, [CSocket], []).

init([CSocket]) ->
    {ok, CPipe} = mc_pipe:start_link(self(), []),
    {ok, SPipe} = mc_pipe:start_link(self(), []),
    ok = mc_pipe:bind(CPipe, CSocket, send),
    ok = mc_pipe:bind(SPipe, CSocket, recv),
    self ! find_server,
    {ok, #state{
        cpipe = CPipe,
        csock = CSocket,
        spipe = SPipe,
        ssock = none
    }}.

-spec send_msg(pid(), Msg) -> ok | timeout when
    Msg :: iodata().
send_msg(Proxy, Msg) ->
    gen_server:call(Proxy, {send_msg, Msg}).

handle_call({send_msg, Msg}, _From, #state{csock = C} = State) ->
    ?LOG_NOTICE(#{event => send_msg, msg => Msg, state => State}),
    mc_socket:send(C, chat_clientbound, #{
        msg => mc_chat:info(Msg),
        position => system_msg,
        sender => <<0:128>>
    }),
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

% Handle if any sockets close unexpectedly
handle_info({mc_error, C, Reason}, #state{csock = C} = State) ->
    ?LOG_NOTICE(#{event => client_conn_closed, reason => Reason, state => State}),
    exit(normal);
handle_info(find_server, #state{cpipe = CPipe, spipe = SPipe} = State) ->
    {ok, SSocket} = mc_server:connect("localhost", 25564, #{name => <<"ttt">>}),
    ok = mc_pipe:bind(CPipe, SSocket, recv),
    ok = mc_pipe:bind(SPipe, SSocket, send),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.
