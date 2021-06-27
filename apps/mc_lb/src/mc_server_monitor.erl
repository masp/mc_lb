-module(mc_server_monitor).
-behaviour(gen_statem).

-include_lib("kernel/include/logger.hrl").

-export([start_link/3]).

%% gen_statem callbacks
-export([
    callback_mode/0,
    init/1,
    handle_event/4,
    terminate/3
]).

% Time to wait in milliseconds trying to connect to a new server. If the process dies and gets restarted by the supervisor,
% then it should wait this BACKOFF_TIME before trying to connect again (and possibly failing). In mc_server_monitor_sup, the maximum restart period should
% be less than this backoff time to guarantee that these processes are never killed because of ping failures (even if they are permanent errors!!).
-define(BACKOFF_TIME, 2000).

% Every 5 seconds heartbeat server to verify they are still alive by sending a status request. We can also then see how many players are online and how
% full the server is.
-define(PING_FREQ, 5000).

-record(data, {
    name :: binary(),
    address :: inet:socket_address() | inet:hostname(),
    port :: inet:port_number()
}).

-spec start_link(Name, Address, Port) -> {ok, pid()} when
    Name :: binary(),
    Address :: inet:socket_address() | inet:hostname(),
    Port :: inet:port_number().
start_link(Name, Address, Port) ->
    gen_statem:start_link(?MODULE, [Name, Address, Port], []).

callback_mode() -> handle_event_function.

init([Name, Address, Port]) ->
    erlang:send_after(?BACKOFF_TIME, self(), ping),
    {ok, connected, #data{
        name = Name,
        address = Address,
        port = Port
    }}.

handle_event(info, ping, connected, Data) ->
    do_status_check(Data),
    erlang:send_after(?PING_FREQ, self(), ping),
    {next_state, connected, Data, []}.

terminate(_Reason, _State, _Data) ->
    ok.

%% Internal functions
do_status_check(#data{name = Name, address = Address, port = Port}) ->
    ?LOG_DEBUG(#{event => doing_status_check, name => Name, address => Address, port => Port}),
    case mc_server:ping(Address, Port) of
        {ok, Info} ->
            mc_server_registry:update(Info#{name => Name});
        {error, Reason} ->
            % Try again in BACKOFF_TIME from supervisor restarting us
            exit({server_not_responding, {error, Reason}})
    end.
