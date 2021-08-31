-module(mc_server_registry).
-behaviour(gen_server).

-include_lib("kernel/include/logger.hrl").

-export([start_link/1, update/1, find_server/1, list_servers/0]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-type server_status() :: online | offline.
-record(server_info, {
    name :: binary(),
    status = offline :: server_status(),
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
        status => server_status(),
        ping => non_neg_integer(),
        max_players => non_neg_integer(),
        online_players => non_neg_integer()
    }.
update(Info) ->
    gen_server:cast(?MODULE, {status_update, Info}).

-spec find_server(Name) -> {ok, {Address, Port}} | server_not_found when
    Name :: binary(),
    Address :: inet:socket_address() | inet:hostname(),
    Port :: inet:port_number().
find_server(Name) ->
    gen_server:call(?MODULE, {find_server, Name}).

-spec list_servers() -> [ServerName] when ServerName :: binary().
list_servers() ->
    [S#server_info.name || S <- ets:tab2list(mc_servers)].

init([ServerList]) ->
    % ETS table where key is `name` in `server_info`
    Tab = ets:new(mc_servers, [ordered_set, {keypos, 2}, named_table]),
    [start_server(S, Tab) || S <- ServerList],
    {ok, #state{servers = Tab}}.

handle_call({find_server, Name}, _From, #state{servers = Tab} = State) ->
    {reply, do_find_server(Tab, Name), State}.

handle_cast(
    {status_update, #{name := Name, status := Status} = Props},
    #state{servers = Servers} = State
) ->
    [Info] = ets:lookup(Servers, Name),
    ets:insert(Servers, Info#server_info{
        status = Status,
        ping = maps:get(ping, Props, 0),
        max_players = maps:get(max_players, Props, 0),
        online_players = maps:get(online_players, Props, 0),
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

do_find_server(Tab, default) ->
    % The default server is always the first server
    do_find_server(Tab, ets:first(Tab));
do_find_server(_Tab, '$end_of_table') ->
    server_not_found;
do_find_server(Tab, Name) ->
    case ets:lookup(Tab, Name) of
        [] -> server_not_found;
        [#server_info{status = offline}] -> {server_offline, Name};
        [#server_info{address = Address, port = Port, status = online}] -> {ok, {Address, Port}}
    end.