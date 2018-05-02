-module(galaxy_structure_util_tests).

-include_lib("eunit/include/eunit.hrl").

-include("galaxy_defs.hrl").
-include("resource_defs.hrl").

-define(STRUCTURE_TYPE_MINE, #structure_type{
                        name = <<"test_mine">>,
                        production_rate = 1,
                        produces = [#resource{
                            name = <<"metal">>,
                            amount = 10}],
                        input_storage_space=0,
                        output_storage_space=1000}).

-define(STRUCTURE_TYPE_FACTORY, #structure_type{
                        name = <<"test_factory">>,
                        production_rate = 3,
                        produces = [#resource{
                            name = <<"spaceship">>,
                            amount = 1}],
                        input_storage_space=500,
                        output_storage_space=100}).

-define(STRUCTURE_TYPE_SMALL_SHIPYARD, #structure_type{
                        name = <<"small_shipyard">>,
                        category = <<"shipyard">>,
                        production_rate = 1,
                        produces = [#resource{
                            name = <<"small_spaceship">>,
                            amount = 1}
                        ],
                        input_storage_space = 1000,
                        output_storage_space = 120,
                        display_name = <<"Test Shipyard">>,
                        metadata = []}).

-define(RESOURCE_TYPE_METAL, #resource_type{
                        name = <<"metal">>,
                        category = <<"metal">>,
                        storage_space = 1,
                        display_name = <<"Generic metal">>,
                        build_materials = [],
                        metadata = []}).

-define(RESOURCE_TYPE_HEAVY_METAL, #resource_type{
                        name = <<"heavy_metal">>,
                        category = <<"metal">>,
                        storage_space = 2,
                        display_name = <<"Generic metal">>,
                        build_materials = [],
                        metadata = []}).

-define(RESOURCE_TYPE_PLASTIC, #resource_type{
                        name = <<"plastic">>,
                        category = <<"plastic">>,
                        storage_space = 1,
                        display_name = <<"Generic plastic">>,
                        build_materials = [],
                        metadata = []}).

-define(RESOURCE_TYPE_SMALL_SPACESHIP, #resource_type{
                        name = <<"small_spaceship">>,
                        category = <<"starfighter">>,
                        storage_space = 10,
                        display_name = <<"Small Starfighter">>,
                        build_materials = [
                            #resource{
                                name = <<"metal">>,
                                amount = 10}
                        ],
                        metadata = []}).

test_setup() ->
    meck:new(galaxy_srv, []),
    meck:expect(galaxy_srv, get_structure_type,
        fun(StructureName) -> 
            case StructureName of
                <<"test_mine">> ->
                    {ok, ?STRUCTURE_TYPE_MINE};
                <<"test_factory">> -> 
                    {ok, ?STRUCTURE_TYPE_FACTORY};
                <<"small_shipyard">> ->
                    {ok, ?STRUCTURE_TYPE_SMALL_SHIPYARD}
            end
        end),
     meck:expect(galaxy_srv, get_resource_type,
        fun(ResourceName) -> 
            case ResourceName of
                <<"metal">> ->
                        {ok, ?RESOURCE_TYPE_METAL};
                <<"heavy_metal">> ->
                        {ok, ?RESOURCE_TYPE_HEAVY_METAL};
                <<"plastic">> -> 
                        {ok, ?RESOURCE_TYPE_PLASTIC};
                <<"small_spaceship">> ->
                        {ok, ?RESOURCE_TYPE_SMALL_SPACESHIP}
                end
            end).

    test_teardown(_Arg) ->
        meck:unload(galaxy_srv).

    fixture_test_() ->
        {
            setup,
            fun test_setup/0, fun test_teardown/1,
            [
                {"Structure over output space capacity",
                    ?_test(test_output_space_over_capacity())},
                {"Structure over output space capacity (heavy resource)",
                    ?_test(test_output_space_over_capacity_heavy())},
                {"Structure over output space capacity (half resource)",
                    ?_test(
                        test_output_space_over_capacity_half_resource())},
                {"Mine resource production",
                    ?_test(test_mine_resource_production())},
                {"Hourly resource rate conversion",
                    ?_test(test_hourly_resource_rate())},
                {"Truncate floating point",
                    ?_test(test_truncate_floating_point())},
                {"Simulate structure",
                    ?_test(test_simulate_structure())}
            ]
        }.

test_mine_resource_production() ->
    TestStructure = #structure{name = <<"test_mine">>},
    Resource = #resource{name = <<"metal">>, amount = 3600},
    Resource2 = #resource{name = <<"heavy_metal">>, amount = 3600},
    OneSecondDeltaTime = 1.0,
    TwoSecondDeltaTime = 2.0,
    % With an amount of 3600 resources per hour, in a second we should
    % have generated exactly 1 resource.
    ?assertEqual(
        {ok, #structure{
            name = <<"test_mine">>,
            output_storage_space = 1.0,
            output_resources = [
                #resource{name = <<"metal">>, amount = 1.0}]}},
        galaxy_structure_util:create_resource(
            Resource,
            ?RESOURCE_TYPE_METAL,
            TestStructure,
            ?STRUCTURE_TYPE_MINE,
            OneSecondDeltaTime)),
    % Now lets produce heavy metal with a storage space of 2,
    % if we create 2 units of heavy  metal we should end up with
    % a used storage space of 4.
    ?assertEqual(
        {ok, #structure{
            name = <<"test_mine">>,
            output_storage_space = 4.0,
            output_resources = [
                #resource{name = <<"heavy_metal">>, amount = 2.0}]}},
        galaxy_structure_util:create_resource(
            Resource2,
            ?RESOURCE_TYPE_HEAVY_METAL,
            TestStructure,
            ?STRUCTURE_TYPE_MINE,
            TwoSecondDeltaTime)).

test_output_space_over_capacity() ->
    UsedSpace = 998,
    MaxSpace = 1000,
    Resource = #resource{
        name = <<"metal">>,
        amount = 10}, 
    ResourceType = #resource_type{
        name = <<"metal">>,
        category = <<"metal">>,
        storage_space = 1,
        display_name = <<"Generic metal">>,
        build_materials = [],
        metadata = []},
    StructureType = #structure_type{
        name = <<"test_structure">>,
        output_storage_space = MaxSpace},
    Structure = #structure{
        uid = ignore,
        name = <<"test_structure">>,
        output_resources = [],
        output_storage_space = UsedSpace},

    ?assertEqual(
        #resource{
            name = <<"metal">>,
            amount = 2.0},
        galaxy_structure_util:cap_output_capacity(
            Structure, StructureType, Resource, ResourceType)).

test_output_space_over_capacity_heavy() ->
    UsedSpace = 998,
    MaxSpace = 1000,
    HeavyResource = #resource{
        name = <<"heavy_metal">>,
        amount = 10}, 
    HeavyResourceType = #resource_type{
        name = <<"metal">>,
        category = <<"metal">>,
        storage_space = 2,
        display_name = <<"Generic metal">>,
        build_materials = [],
        metadata = []},
    StructureType = #structure_type{
        name = <<"test_structure">>,
        output_storage_space = MaxSpace},
    Structure = #structure{
        uid = ignore,
        name = <<"test_structure">>,
        output_resources = [],
        output_storage_space = UsedSpace},

    % Since heavy metal takes 2 storage space, we can only fit
    % 1 unit of the resource.
    ?assertEqual(
        #resource{
            name = <<"heavy_metal">>,
            amount = 1.0},
        galaxy_structure_util:cap_output_capacity(
            Structure, StructureType, HeavyResource, HeavyResourceType)).

test_output_space_over_capacity_half_resource() ->
    UsedSpace = 999,
    MaxSpace = 1000,
    HeavyResource = #resource{
        name = <<"heavy_metal">>,
        amount = 10}, 
    HeavyResourceType = #resource_type{
        name = <<"metal">>,
        category = <<"metal">>,
        storage_space = 2,
        display_name = <<"Generic metal">>,
        build_materials = [],
        metadata = []},
    StructureType = #structure_type{
        name = <<"test_structure">>,
        output_storage_space = MaxSpace},
    Structure = #structure{
        uid = ignore,
        name = <<"test_structure">>,
        output_resources = [],
        output_storage_space = UsedSpace},

    % Since heavy metal takes 2 storage space, we can only fit
    % 0.5 unit of the resource with 1 space.
    ?assertEqual(
        #resource{
            name = <<"heavy_metal">>,
            amount = 0.5},
        galaxy_structure_util:cap_output_capacity(
            Structure, StructureType, HeavyResource, HeavyResourceType)).

test_hourly_resource_rate() ->
    OneSecondDeltaTime = 1.0,
    OneAndHalfSecondDeltaTime = 1.5,
    ThreePointSevenSecondDeltaTime = 3.7,
    ?assertEqual(
        0.027777,
        galaxy_util:hourly_resource_rate(
            100, OneSecondDeltaTime)),
    ?assertEqual(
        0.002777,
        galaxy_util:hourly_resource_rate(
            10, OneSecondDeltaTime)),
    ?assertEqual(
        0.004166,
        galaxy_util:hourly_resource_rate(
            10, OneAndHalfSecondDeltaTime)),
    ?assertEqual(
        0.263111,
        galaxy_util:hourly_resource_rate(
            256, ThreePointSevenSecondDeltaTime)).

test_truncate_floating_point() ->
    ?assertEqual(
        6.6666,
        galaxy_util:truncate(6.6666666666, 4)),
    ?assertEqual(
        101.34,
        galaxy_util:truncate(101.34123456789, 2)).

test_simulate_structure() ->
   Structure = #structure{
        name = <<"small_shipyard">>,
        output_resources = [],
        input_resources = [#resource{
            name = <<"metal">>,
            amount = 10}],
        output_storage_space = 0,
        input_storage_space = 0},
    ExpectedStructure = #structure{
            name = <<"small_shipyard">>,
            output_resources = [
                #resource{
                    name = <<"small_spaceship">>,
                    amount = 1}
            ],
            input_resources = [],
            input_storage_space = 0,
            output_storage_space = 10},
    DeltaTime = 1.0,
    {ok, Result}  = galaxy_structure_util:simulate_structure(Structure,
        DeltaTime),
    ?assertEqual(ExpectedStructure, Result).
