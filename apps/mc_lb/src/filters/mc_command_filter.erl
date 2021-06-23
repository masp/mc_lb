-module(mc_command_filter).
-behaviour(mc_proxy_filter).

-include_lib("kernel/include/logger.hrl").

-export([init/1, handle_filter/3]).

init(_Args) ->
    none.

handle_filter({chat_serverbound, #{message := <<"/", CmdAll/binary>>}}, Proxy, State) ->
    Args = binary:split(CmdAll, <<" ">>, [trim_all]),
    case handle_command(Args, Proxy) of
        ok ->
            ?LOG_NOTICE(#{event => cmd, command => CmdAll}),
            {block, State};
        unknown_cmd ->
            {forward, State}
    end;
handle_filter({_PacketName, _Packet}, _Proxy, State) ->
    {forward, State}.

handle_command([<<"worlds">>, <<"list">>], Proxy) ->
    mc_proxy:send_msg(Proxy, <<"World list:">>);
handle_command(_Args, _Proxy) ->
    unknown_cmd.
