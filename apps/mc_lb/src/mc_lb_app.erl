%%%-------------------------------------------------------------------
%% @doc mc_lb public API
%% @end
%%%-------------------------------------------------------------------

-module(mc_lb_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    mc_lb_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
