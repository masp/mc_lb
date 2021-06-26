%%%-------------------------------------------------------------------
%% @doc mc_lb top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(mc_lb_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([]) ->
    SupFlags = #{
        strategy => one_for_one,
        intensity => 1,
        period => 5
    },
    ChildSpecs = [
        #{
            id => mc_listener,
            start => {mc_players_sup, start_link, []},
            restart => permanent,
            type => supervisor,
            modules => [mc_player_sup]
        },
        #{
            id => mc_servers_sup,
            start => {mc_servers_sup, start_link, []},
            restart => permanent,
            type => supervisor,
            modules => [mc_servers_sup]
        }
    ],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
