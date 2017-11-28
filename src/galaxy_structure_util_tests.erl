-module(galaxy_structure_util_tests).

-include_lib("eunit/include/eunit.hrl").

-include("galaxy_defs.hrl").

-define(STRUCTURE_MINE_TYPE, #structure_type{
                        name = <<"test_mine">>,
                        rate = 1,
                        produces = [#resource{
                            name = <<"metal">>,
                            amount = 10}],
                        input_storage_space=0,
                        output_storage_space=1000}).

-define(STRUCTURE_FACTORY_TYPE, #structure_type{
                        name = <<"test_factory">>,
                        rate = 3,
                        produces = [#resource{
                            name = <<"spaceship">>,
                            amount = 1}],
                        input_storage_space=500,
                        output_storage_space=100}).

-define(RESOURCE_METAL_TYPE, #resource_type{
                        name = <<"metal">>,
                        category = <<"metal">>,
                        storage_space = 1,
                        display_name = <<"Generic metal">>,
                        build_materials = [],
                        metadata = []}).

-define(RESOURCE_HEAVY_METAL_TYPE, #resource_type{
                        name = <<"heavy_jmetal">>,
                        category = <<"metal">>,
                        storage_space = 2,
                        display_name = <<"Generic metal">>,
                        build_materials = [],
                        metadata = []}).

-define(RESOURCE_PLASTIC_TYPE, #resource_type{
                        name = <<"plastic">>,
                        category = <<"plastic">>,
                        storage_space = 1,
                        display_name = <<"Generic plastic">>,
                        build_materials = [],
                        metadata = []}).

test_setup() ->
    meck:new(galaxy_srv, []),
    meck:expect(galaxy_srv, get_structure_type,
        fun(StructureName) -> 
            case StructureName of
                <<"test_mine">> ->
                    {ok, ?STRUCTURE_MINE_TYPE};
                <<"test_factory">> -> 
                    {ok, ?STRUCTURE_FACTORY_TYPE}
            end
        end),
     meck:expect(galaxy_srv, get_resource_type,
        fun(ResourceName) -> 
            case ResourceName of
                <<"metal">> ->
                        {ok, ?RESOURCE_METAL_TYPE};
                <<"heavy_metal">> ->
                        {ok, ?RESOURCE_HEAVY_METAL_TYPE};
                <<"plastic">> -> 
                        {ok, ?RESOURCE_PLASTIC_TYPE}
                end
            end).

    test_teardown(_Arg) ->
        meck:unload(galaxy_srv).

    fixture_test_() ->
        {
            setup,
            fun test_setup/0, fun test_teardown/1,
            [
                {"Add resource in empty list",
                    ?_test(test_add_resource_in_empty_list())},
                {"Add resource in existing list", 
                    ?_test(test_add_resource_in_existing_list())},
                {"Structure over output space capacity",
                    ?_test(test_output_space_over_capacity())},
                {"Mine resource production",
                    ?_test(test_mine_resource_production())},
                {"Hourly resource rate conversion",
                    ?_test(test_hourly_resource_rate())}
            ]
        }.

    test_add_resource_in_empty_list() ->
       ?assertEqual(
            [#resource{name = <<"metal">>, amount = 10}],
            galaxy_structure_util:add_resource_to_list(
                #resource{
                    name = <<"metal">>,
                    amount = 10
                },
                [])).

    test_add_resource_in_existing_list() ->
        ?assertEqual(
            [
                #resource{name = <<"plastic">>, amount = 15},
                #resource{name = <<"metal">>, amount = 10}
            ],
            galaxy_structure_util:add_resource_to_list(
                #resource{name = <<"metal">>, amount = 10},
                [#resource{name = <<"plastic">>, amount = 15}])).

test_mine_resource_production() ->
    TestStructure = #structure{name = <<"test_mine">>},
    ResourceList = [#resource{name = <<"metal">>, amount = 3600}],
    ResourceList2 = [#resource{name = <<"heavy_metal">>,
        amount = 3600}],
    OneSecondDeltaTime = 1.0,
    TwoSecondDeltaTime = 2.0,
    % With an amount of 3600 resources per hour, in a second we should
    % have generated exactly 1 resource.
    ?assertEqual(
        #structure{
            name = <<"test_mine">>,
            output_storage_space = 1.0,
            output_resources = [
                #resource{name = <<"metal">>, amount = 1.0}]},
        galaxy_structure_util:convert_resources(ResourceList,
            TestStructure, OneSecondDeltaTime)),
    % Now lets produce heavy metal with a storage space of 2,
    % if we create 2 units of heavy  metal we should end up with
    % a used storage space of 4.
    ?assertEqual(
        #structure{
            name = <<"test_mine">>,
            output_storage_space = 4.0,
            output_resources = [
                #resource{name = <<"heavy_metal">>, amount = 2.0}]},
        galaxy_structure_util:convert_resources(ResourceList2,
            TestStructure, TwoSecondDeltaTime)).

test_output_space_over_capacity() ->
    UsedSpace = 998,
    Resource = #resource{
        name = <<"metal">>,
        amount = 10}, 

    HeavyResource = #resource{
        name = <<"heavy_metal">>,
        amount = 10}, 

    StructureType = #structure_type{
        name = <<"test_structure">>,
        output_storage_space=1000},

    Structure = #structure{
        uid = ignore,
        name = <<"test_structure">>,
        output_resources = [],
        output_storage_space = UsedSpace},

    ?assertEqual(2, galaxy_structure_util:cap_output_capacity(
        Structure, StructureType, Resource)),

    ?assertEqual(2, galaxy_structure_util:cap_output_capacity(
        Structure, StructureType, HeavyResource)).
    
test_hourly_resource_rate() ->
    OneSecondDeltaTime = 1.0,
    OneAndHalfSecondDeltaTime = 1.5,
    ThreePointSevenSecondDeltaTime = 3.7,
    ?assertEqual(
        0.027777,
        galaxy_structure_util:hourly_resource_rate(
            100, OneSecondDeltaTime)),
    ?assertEqual(
        0.002777,
        galaxy_structure_util:hourly_resource_rate(
            10, OneSecondDeltaTime)),
    ?assertEqual(
        0.004166,
        galaxy_structure_util:hourly_resource_rate(
            10, OneAndHalfSecondDeltaTime)),
    ?assertEqual(
        0.263111,
        galaxy_structure_util:hourly_resource_rate(
            256, ThreePointSevenSecondDeltaTime)).
