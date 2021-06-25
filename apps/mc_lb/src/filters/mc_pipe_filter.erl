-module(mc_pipe_filter).

-export([filter/4]).

-callback handle_filter(Player, {Name, Packet}) -> Action when
    Name :: mc_protocol:packet_name(),
    Packet :: map(),
    Player :: mc_player:player(),
    Action :: forward | block.

-spec filter(Filter, Player, PacketName, Packet) -> forward | block when
    Filter :: module(),
    Player :: mc_player:player(),
    PacketName :: mc_protocol:packet_name(),
    Packet :: map().
filter(FMod, Player, Name, Packet) ->
    FMod:handle_filter(Player, {Name, Packet}).

%% Internal functions
