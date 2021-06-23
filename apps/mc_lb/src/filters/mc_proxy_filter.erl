-module(mc_proxy_filter).
-behaviour(gen_server).

-export([start_link/3, filter/3]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-callback init(Args) -> State when
    Args :: [term()],
    State :: term().

-callback handle_filter({Name, Packet}, Proxy, State) -> Action when
    Name :: mc_protocol:packet_name(),
    Packet :: map(),
    Proxy :: pid(),
    State :: term(),
    Action :: forward | block.

-record(state, {
    mod :: module(),
    proxy :: pid(),
    fstate :: term()
}).

start_link(Mod, Proxy, Args) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Mod, Proxy, Args], []).

-spec filter(Filter, Name, Packet) -> forward | block when
    Filter :: term(),
    Name :: mc_protocol:packet_name(),
    Packet :: map().
filter(Ref, PacketName, Packet) ->
    gen_server:call(Ref, {filter, PacketName, Packet}).

init([FilterMod, Proxy, Args]) ->
    FilterState = FilterMod:init(Proxy, Args),
    {ok, #state{mod = FilterMod, proxy = Proxy, fstate = FilterState}}.

handle_call(
    {filter, PacketName, Packet},
    _From,
    #state{proxy = Proxy, mod = FMod, fstate = FState} = State
) ->
    {Action, NewFState} = FMod:handle_filter({PacketName, Packet}, Proxy, FState),
    {reply, Action, State#state{fstate = NewFState}}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal functions
