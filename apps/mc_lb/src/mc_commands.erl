-module(mc_commands).

-include_lib("kernel/include/logger.hrl").

-export([parse_command/1]).

parse_command(<<"/", CmdAll/binary>>) ->
    binary:split(CmdAll, <<" ">>, [global, trim_all]);
parse_command(_Msg) ->
    not_command.
