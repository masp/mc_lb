-module(mc_server_monitor_sup).
-behaviour(supervisor).

-export([start_link/1]).
-export([init/1]).

start_link(Servers) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [Servers]).

init([Servers]) ->
    SupFlags = #{
        strategy => simple_one_for_one,
        intensity => 0,
        period => 1
    },
    ChildSpecs = [
        #{
            id => ServerName,
            start => {mc_server_monitor, start_link, [S]}
        }
     || S = #{name := ServerName} <- Servers
    ],
    {ok, {SupFlags, ChildSpecs}}.
