-module(mc_server_registry).
-behaviour(gen_server).

-export([start_link/1]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-type server_info() :: #{
    name => binary(),
    address => inet:socket_address() | inet:hostname(),
    port => inet:port_number(),
    is_alive => boolean(),
    ping => non_neg_integer()
}.

-record(state, {
    servers :: #{Name :: binary() => server_info()}
}).

start_link(Servers) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Servers], []).

init([ServerList]) ->
    Servers = maps:from_list([{Name, Server} || #{name := Name} = Server <- ServerList]),
    {ok, #state{servers = Servers}}.

handle_call(
    {status_update, ServerName, IsAlive, Ping},
    _From,
    #state{servers = Servers} = State
) ->
    #{ServerName := PrevInfo} = Servers,
    NewServers = Servers#{ServerName := PrevInfo#{is_alive => IsAlive, ping => Ping}},
    {noreply, State#state{servers = NewServers}};
handle_call(get_servers, _From, #state{servers = Servers} = State) ->
    {reply, {servers, Servers}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(ping_servers, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal functions
