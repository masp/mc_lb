-module(mc_pipe).
-behaviour(gen_server).

-export([start_link/0, start_link/1, bind/3]).

% mc_pipe is a process that transfer Minecraft packets from one socket to another in one direction. It also can be told
% to filter out select packets and send them to a listener process. Those packets can then be held back or sent
% through the pipe again.
%
% To connect a Socket1 to Socket2, do:
%
% Pipe = mc_pipe:start_link(#{join_game => self()}).
% mc_pipe:bind(Pipe, recv, Socket1).
% mc_pipe:bind(Pipe, send, Socket2).
%
% mc_socket:send(Socket1, join_game, #{...}).
% flush().
%   Received {filtered_packet, <Pid>, join_game, #{...}).

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
-type filters() :: #{mc_protocol:packet_name() => pid()}.

-opaque pipe() :: pid().
-export_type([pipe/0]).

-record(state, {
    recv = none :: proxy_socket(),
    send = none :: proxy_socket(),
    filters = #{} :: filters()
}).

-spec start_link() -> {ok, mc_pipe:pipe()}.
start_link() ->
    start_link(#{}).

-spec start_link(Filters) -> {ok, mc_pipe:pipe()} when
    Filters :: filters().
start_link(Filters) ->
    gen_server:start_link(?MODULE, [Filters], []).

-spec bind(pipe(), Side, Socket) -> ok when
    Side :: recv | send,
    Socket :: mc_socket:socket().
bind(Pipe, Side, Socket) ->
    gen_server:call(Pipe, {bind, Side, Socket}).

init([Filters]) ->
    {ok, #state{filters = Filters}}.

handle_call({bind, recv, Socket}, _From, State) ->
    ok = mc_socket:recv_passive(Socket),
    {reply, ok, State#state{recv = Socket}};
handle_call({bind, send, Socket}, _From, State) ->
    {reply, ok, State#state{send = Socket}}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(
    {R, {packet, Name, Packet}},
    #state{recv = R, send = S, filters = Filters} = State
) when S =/= none ->
    case maps:get(Name, Filters, nobody) of
        nobody ->
            ok = mc_socket:send(S, Name, Packet);
        Listener ->
            Listener ! {filtered_packet, self(), Name, Packet}
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
