-module(mc_player_sup).

-behavior(supervisor).

-export([start_link/1]).
-export([init/1]).

start_link(CSocket) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [CSocket]).

init([CSocket]) ->
    SupFlags = #{
        strategy => one_for_one,
        intensity => 0,
        period => 1
    },
    ChildSpecs = [
        % Acceptor
        #{
            id => {mc_player, make_ref()},
            start => {mc_player, start_link, [CSocket]},
            restart => transient
        }
    ],
    {ok, {SupFlags, ChildSpecs}}.
