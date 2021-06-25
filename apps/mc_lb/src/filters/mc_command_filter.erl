-module(mc_command_filter).
-behaviour(mc_pipe_filter).

-include_lib("kernel/include/logger.hrl").

-export([handle_filter/2]).

handle_filter(Player, {chat_serverbound, #{message := <<"/", CmdAll/binary>>}}) ->
    Args = binary:split(CmdAll, <<" ">>, [trim_all]),
    case handle_command(Args, Player) of
        ok ->
            ?LOG_NOTICE(#{event => cmd, command => CmdAll}),
            block;
        unknown_cmd ->
            forward
    end;
handle_filter(_Player, {_PacketName, _Packet}) ->
    forward.

handle_command([<<"worlds">>, <<"list">>], Player) ->
    mc_proxy:send_msg(Player, <<"World list:">>);
handle_command(_Args, _Proxy) ->
    unknown_cmd.
