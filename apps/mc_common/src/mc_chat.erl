-module(mc_chat).
-export([warn/1, info/1]).

warn(Msg) ->
    jsone:encode(#{
        text => Msg,
        color => <<"red">>
    }).

info(Msg) ->
    jsone:encode(#{text => Msg}).
