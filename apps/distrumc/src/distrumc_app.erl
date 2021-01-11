%%%-------------------------------------------------------------------
%% @doc distrumc public API
%% @end
%%%-------------------------------------------------------------------

-module(distrumc_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    distrumc_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
