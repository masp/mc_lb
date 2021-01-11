-module(mc_registry).

-export([default_dimension/0, default_dimension_codec/0]).

default_dimension_codec() ->
    {tag_compound, <<>>, DefaultDim} = default_dimension(),
    {tag_compound, <<>>, [
        {tag_compound, <<"minecraft:dimension_type">>, [
            {tag_string, <<"type">>, <<"minecraft:dimension_type">>},
            {tag_list, <<"value">>,
                {tag_compound, [
                    [
                        {tag_string, <<"name">>, <<"minecraft:overworld">>},
                        {tag_int, <<"id">>, 0},
                        {tag_compound, <<"element">>, DefaultDim}
                    ]
                ]}}
        ]},
        {tag_compound, <<"minecraft:worldgen/biome">>, [
            {tag_string, <<"type">>, <<"minecraft:worldgen/biome">>},
            {tag_list, <<"value">>,
                {tag_compound, [
                    [
                        {tag_string, <<"name">>, <<"minecraft:plains">>},
                        {tag_int, <<"id">>, 0},
                        {tag_compound, <<"element">>, [
                            {tag_string, <<"precipitation">>, <<"rain">>},
                            {tag_float, <<"depth">>, 0.125},
                            {tag_float, <<"temperature">>, 0.8},
                            {tag_float, <<"scale">>, 0.05},
                            {tag_float, <<"downfall">>, 0.4},
                            {tag_string, <<"category">>, <<"plains">>},
                            {tag_compound, <<"effects">>, [
                                {tag_int, <<"sky_color">>, 7907327},
                                {tag_int, <<"water_fog_color">>, 329011},
                                {tag_int, <<"fog_color">>, 12638463},
                                {tag_int, <<"water_color">>, 4159204},
                                {tag_compound, <<"effects">>, [
                                    {tag_int, <<"tick_delay">>, 6000},
                                    {tag_double, <<"offset">>, 2.0},
                                    {tag_string, <<"sound">>, <<"minecraft:ambient.cave">>},
                                    {tag_int, <<"block_search_extent">>, 8}
                                ]}
                            ]}
                        ]}
                    ]
                ]}}
        ]}
    ]}.

default_dimension() ->
    {tag_compound, <<>>, [
        {tag_byte, <<"piglin_safe">>, 0},
        {tag_byte, <<"natural">>, 1},
        {tag_float, <<"ambient_light">>, 0},
        {tag_string, <<"infiniburn">>, <<"minecraft:infiniburn_overworld">>},
        {tag_byte, <<"respawn_anchor_works">>, 0},
        {tag_byte, <<"has_skylight">>, 1},
        {tag_byte, <<"bed_works">>, 1},
        {tag_string, <<"effects">>, <<"minecraft:overworld">>},
        {tag_byte, <<"has_raids">>, 1},
        {tag_int, <<"logical_height">>, 256},
        {tag_double, <<"coordinate_scale">>, 1.0},
        {tag_byte, <<"ultrawarm">>, 0},
        {tag_byte, <<"has_ceiling">>, 0}
    ]}.
