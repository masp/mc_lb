-module(mc_server_monitor_sup).
-behaviour(supervisor).

-include_lib("kernel/include/logger.hrl").

-export([start_link/0, start_monitor/3]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    SupFlags = #{
        strategy => simple_one_for_one,
        intensity => 1,
        % When a monitor dies, it waits three seconds before restarting. This supervisor should never die.
        period => 3
    },
    % Children are started dynamically from mc_server_registry
    ChildSpecs = [
        #{
            id => mc_server_monitor,
            start => {mc_server_monitor, start_link, []},
            restart => permanent,
            type => worker
        }
    ],
    {ok, {SupFlags, ChildSpecs}}.

-spec start_monitor(Name, Address, Port) -> {ok, pid()} when
    Name :: binary(),
    Address :: inet:socket_address() | inet:hostname(),
    Port :: inet:port_number().
start_monitor(Name, Address, Port) ->
    supervisor:start_child(?MODULE, [Name, Address, Port]).
