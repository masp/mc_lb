-module(mc_server_registry).
-behaviour(gen_server).

-include_lib("kernel/include/logger.hrl").

-export([start_link/1, update/1]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-record(server_info, {
    name :: binary(),
    address :: inet:socket_address() | inet:hostname(),
    port :: inet:port_number(),
    last_pinged = 0 :: integer(),
    ping = 0 :: non_neg_integer(),
    online_players = 0 :: non_neg_integer(),
    max_players = 0 :: non_neg_integer()
}).

-record(state, {
    servers :: ets:tab()
}).

-type server_conn_info() :: #{
    name => binary(),
    address => inet:socket_address() | inet:hostname(),
    port => inet:port_number()
}.

-spec start_link(ServerList :: [server_conn_info()]) -> {ok, pid()}.
start_link(ServerList) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [ServerList], []).

-spec update(Info) -> ok when
    Info :: #{
        name => binary(),
        ping => non_neg_integer(),
        max_players => non_neg_integer(),
        online_players => non_neg_integer()
    }.
update(Info) ->
    gen_server:cast(?MODULE, {status_update, Info}).

init([ServerList]) ->
    % ETS table where key is `name` in `server_info`
    Tab = ets:new(servers, [set, {keypos, 2}]),
    [start_server(S, Tab) || S <- ServerList],
    {ok, #state{servers = Tab}}.

handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_cast(
    {status_update, #{
        name := Name,
        ping := Ping,
        max_players := MaxPlayers,
        online_players := OnlinePlayers
    }},
    #state{servers = Servers} = State
) ->
    [Info] = ets:lookup(Servers, Name),
    ets:insert(Servers, Info#server_info{
        ping = Ping,
        max_players = MaxPlayers,
        online_players = OnlinePlayers,
        last_pinged = erlang:system_time()
    }),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal functions

start_server(#{name := Name, address := Address, port := Port}, Tab) ->
    ?LOG_NOTICE(#{event => monitoring_server, name => Name, address => Address, port => Port}),
    ets:insert_new(Tab, #server_info{
        name = Name,
        address = Address,
        port = Port
    }),
    mc_server_monitor_sup:start_monitor(Name, Address, Port).
