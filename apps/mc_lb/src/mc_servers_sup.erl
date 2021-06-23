-module(mc_servers_sup).
-behavior(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_Args) ->
    SupFlags = #{
        strategy => one_for_one,
        intensity => 0,
        period => 1
    },
    ChildSpecs = [
        % Main Server Registry
        #{
            id => mc_server_registry,
            start => {mc_server_registry, start_link, [servers()]}
        },
        #{
            id => mc_server_monitor_sup,
            start => {mc_server_monitor_sup, start_link, [servers()]}
        }
    ],
    {ok, {SupFlags, ChildSpecs}}.

servers() ->
    [
        #{name => <<"Server A">>, address => "localhost", port => 25564},
        #{name => <<"Server B">>, address => "localhost", port => 25563}
    ].
