-module(mc_servers_sup).
-behavior(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_Args) ->
    SupFlags = #{strategy => one_for_one},
    ChildSpecs = [
        % Main Server Registry
        #{
            id => mc_server_monitor_sup,
            start => {mc_server_monitor_sup, start_link, []},
            restart => permanent,
            type => supervisor
        },
        #{
            id => mc_server_registry,
            start => {mc_server_registry, start_link, [servers()]},
            restart => permanent,
            type => worker
        }
    ],
    {ok, {SupFlags, ChildSpecs}}.

servers() ->
    [
        #{name => <<"A">>, address => "localhost", port => 25564},
        #{name => <<"B">>, address => "localhost", port => 25563}
    ].
