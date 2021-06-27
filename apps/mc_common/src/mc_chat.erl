-module(mc_chat).
-export([warn/1, info/1]).

-spec warn(Msg) -> Json when Msg :: iolist(), Json :: binary().
warn(Msg) ->
    jsone:encode(#{
        text => iolist_to_binary(Msg),
        color => <<"red">>
    }).

-spec info(Msg) -> Json when Msg :: iolist(), Json :: binary().
info(Msg) ->
    jsone:encode(#{text => iolist_to_binary(Msg)}).
