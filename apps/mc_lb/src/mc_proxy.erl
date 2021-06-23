-module(mc_proxy).
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
    client = mc_socket:socket(),
    server = mc_socket:socket(),
    filters = [] :: [module()]
}).

-spec start_link(ClientSocket :: mc_socket:socket()) -> {ok, pid()}.
start_link(CSocket) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [CSocket], []).

init([CSocket]) ->
    {ok, SSocket} = mc_server:connect("localhost", 25564, #{name => <<"ttt">>}),
    ok = mc_socket:controlling_process(CSocket, self()).
mc_socket:set_active(SSocket),
mc_socket:set_active(CSocket),
{ok, #state{
    client = CSocket,
    server = SSocket,
    filters = [
        mc_command_filter
    ]
}}.

-spec send_msg(pid(), Msg) -> ok | timeout when
    Msg :: iodata().
send_msg(Proxy, Msg) ->
    gen_server:call(Proxy, {send_msg, Msg}).

handle_call({send_msg, Msg}, _From, #state{client = C} = State) ->
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
handle_info({mc_error, C, Reason}, #state{client = C} = State) ->
    ?LOG_NOTICE(#{event => client_conn_closed, reason => Reason, state => State}),
    exit(normal);
%
% Proxy all known packets
handle_info({packet, C, {Name, Packet}}, #state{client = C, server = S, filters = Fs} = State) ->
    mc_socket:set_active(C),
    case run_filters(Name, Packet, Fs) of
        forward ->
            ok = mc_socket:send(S, Name, Packet)
    end,
    {noreply, State};
handle_info({packet, S, {Name, Packet}}, #state{client = C, server = S, filters = Fs} = State) ->
    mc_socket:set_active(S),
    case run_filters(Name, Packet, Fs) of
        forward ->
            ok = mc_socket:send(C, Name, Packet)
    end,
    {noreply, State};
%
% Proxy all unknown packets
handle_info({unknown_packet, C, {ID, PacketBin}}, #state{client = C, server = S} = State) ->
    mc_socket:set_active(C),
    ok = mc_socket:send_raw(S, ID, PacketBin),
    {noreply, State};
handle_info({unknown_packet, S, {ID, PacketBin}}, #state{client = C, server = S} = State) ->
    mc_socket:set_active(S),
    ok = mc_socket:send_raw(C, ID, PacketBin),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

run_filters(_Name, _Packet, []) ->
    forward;
run_filters(Name, Packet, [F | Filters]) ->
    case mc_proxy_filter:filter(F, Name, Packet) of
        block ->
            block;
        forward ->
            run_filters(Name, Packet, Filters)
    end.
