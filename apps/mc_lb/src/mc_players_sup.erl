-module(mc_players_sup).

-behavior(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_Args) ->
    {ok, LSocket} = gen_tcp:listen(25565, [binary, {packet, 0}, {active, false}, {reuseaddr, true}]),
    SupFlags = #{
        strategy => one_for_one,
        intensity => 0,
        period => 1
    },
    ChildSpecs = [
        % Acceptor
        #{
            id => mc_tcp_acceptor,
            start => {mc_acceptor, start_link, [LSocket]}
        }
        % Temporary children will be started for each connection that the acceptor creates
    ],
    {ok, {SupFlags, ChildSpecs}}.
