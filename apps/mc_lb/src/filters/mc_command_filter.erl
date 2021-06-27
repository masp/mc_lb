-module(mc_command_filter).
-behaviour(mc_pipe_filter).

-include_lib("kernel/include/logger.hrl").

-export([handle_filter/2]).

handle_filter(Player, {chat_serverbound, #{message := <<"/", CmdAll/binary>>}}) ->
    Args = binary:split(CmdAll, <<" ">>, [global, trim_all]),
    ?LOG_NOTICE(#{event => cmd, command => CmdAll, args => Args}),
    case handle_command(Args, Player) of
        ok ->
            block;
        unknown_cmd ->
            forward
    end;
handle_filter(_Player, {_PacketName, _Packet}) ->
    forward.

handle_command([<<"worlds">>, <<"list">>], Player) ->
    mc_player:send_msg(Player, <<"World list:">>),
    [mc_player:send_msg(Player, ["- ", S]) || S <- mc_server_registry:list_servers()],
    ok;
handle_command([<<"worlds">>, <<"go">>, Name], Player) ->
    mc_player:send_msg(Player, ["Switching to server ", Name]),
    mc_player:switch_server(Player, Name),
    ok;
handle_command(_Args, _Proxy) ->
    unknown_cmd.
