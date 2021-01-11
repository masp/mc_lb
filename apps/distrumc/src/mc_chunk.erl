-module(mc_chunk).

-export([generate_chunk_column/1, encode_chunk_column/2]).

-define(AIR, 0).
-define(SMOOTH_STONE, 1).
-define(BEDROCK, 33).
-define(GRASS, 1342).
-define(BITS_PER_BLOCK, 14).
-define(BLOCKS_PER_LAYER, (16 * 16)).
-define(BLOCKS_PER_SECTION, (?BLOCKS_PER_LAYER * 16)).
-define(BLOCKS_PER_LONG, (64 div ?BITS_PER_BLOCK)).
-define(LONGS_PER_LAYER, (?BLOCKS_PER_LAYER div ?BLOCKS_PER_LONG)).
-define(LONGS_PER_SECTION, (?BLOCKS_PER_SECTION div ?BLOCKS_PER_LONG)).

id_to_long(ID) ->
    PaddingLen = 64 rem ?BITS_PER_BLOCK,
    IDs = <<<<ID:?BITS_PER_BLOCK/unsigned-integer>> || _ <- lists:seq(1, ?BLOCKS_PER_LONG)>>,
    <<0:PaddingLen/integer, IDs/binary>>.

empty_chunk_section() ->
    #{
        non_air_blocks => 0,
        blocks => binary:copy(id_to_long(?AIR), ?LONGS_PER_SECTION),
        block_light => binary:copy(<<255>>, ?BLOCKS_PER_SECTION div 2),
        sky_light => binary:copy(<<255>>, ?BLOCKS_PER_SECTION div 2)
    }.

generate_chunk_column({X, Z}) ->
    io:format("generating chunk ~p~n", [{chunk, X, Z}]),
    EmptyChunk = empty_chunk_section(),
    BedrockChunk = EmptyChunk#{
        non_air_blocks => ?BLOCKS_PER_SECTION,
        blocks => list_to_binary([
            binary:copy(id_to_long(?BEDROCK), ?LONGS_PER_LAYER),
            binary:copy(id_to_long(?SMOOTH_STONE), ?LONGS_PER_LAYER * 15)
        ])
    },
    StoneChunk = EmptyChunk#{
        non_air_blocks => ?BLOCKS_PER_SECTION,
        blocks => binary:copy(id_to_long(?SMOOTH_STONE), ?LONGS_PER_SECTION)
    },
    TopChunk = EmptyChunk#{
        non_air_blocks => ?BLOCKS_PER_SECTION,
        blocks => list_to_binary([
            binary:copy(id_to_long(?SMOOTH_STONE), ?LONGS_PER_LAYER * 15),
            binary:copy(id_to_long(?GRASS), ?LONGS_PER_LAYER)
        ])
    },
    #{
        pos => {X, Z},
        sections => #{
            3 => TopChunk,
            2 => StoneChunk,
            1 => StoneChunk,
            0 => BedrockChunk
        },
        biome_ids => lists:duplicate(1024, 0)
    }.

dummy_heightmap() ->
    {tag_compound, <<>>, [
        {tag_long_array, <<"MOTION_BLOCKING">>, lists:duplicate(36, 0)}
    ]}.

encode_section(#{blocks := Blocks, non_air_blocks := NonAirBlocks}, _Protocol) ->
    [
        mc_protocol:encode_field(i16, NonAirBlocks),
        mc_protocol:encode_field(u8, ?BITS_PER_BLOCK),
        Blocks
    ].

encode_sections(Sections, Protocol) ->
    NonAirSections = lists:sort(maps:keys(Sections)),
    Lst = list_to_binary(
        lists:map(fun(V) -> encode_section(maps:get(V, Sections), Protocol) end, NonAirSections)
    ),
    Lst.

encode_chunk_column(#{pos := {X, Z}, sections := Sections, biome_ids := Biomes}, Protocol) ->
    EncSections = encode_sections(Sections, Protocol),
    #{
        chunk_x => X,
        chunk_z => Z,
        full_chunk => true,
        primary_bitmask => maps:keys(Sections),
        heightmaps => dummy_heightmap(),
        biomes => Biomes,
        block_entities => [],
        data => EncSections
    }.
