-module(protocol_definition).

-callback version() -> VersionNum :: mc_protocol:version().

-callback lookup_packet_info(PacketInfo) -> Result when
    PacketInfo ::
        {name, {ID, State, Dir}}
        | {id, {Name, Dir}},
    Result :: Name | ID | {unknown_packet, PacketInfo},
    ID :: mc_protocol:packet_id(),
    Name :: mc_protocol:packet_name(),
    State :: mc_protocol:packet_state(),
    Dir :: mc_protocol:packet_dir().

-callback packet(PacketName :: mc_protocol:packet_name()) ->
    PacketDef :: mc_protocol:packet_definition().
