-module(battle_srv_tests).

-include_lib("eunit/include/eunit.hrl").

-include("battle_defs.hrl").
-include("../resource_srv/include/resource_defs.hrl").

-define(GALAXY_ID, <<"battle_srv_test">>).

test_setup() ->
    meck:new(mnesia_battle, []),
    meck:new(resource_srv, [non_strict]),
    mock_functions(),
    ok.

mock_functions() ->
    meck:expect(resource_srv, get_resource_types,
                fun get_resource_types_mock/2),
    meck:expect(resource_srv, get_resource_type,
                fun get_resource_type_mock/2),

    meck:expect(mnesia_battle, get_weapon_type,
                fun get_weapon_type_mock/3),
    meck:expect(mnesia_battle, get_weapon_types,
                fun get_weapon_types_mock/2),
    meck:expect(mnesia_battle, get_weapon_types,
                fun get_weapon_types_mock/3),
    ok.

test_teardown(_Arg) ->
    meck:unload(mnesia_battle),
    meck:unload(resource_srv),
    ok.

get_weapon_type_mock(Name, GalaxyId, State) ->
    {ok, AllWeapons} = get_weapon_types_mock(GalaxyId, State),
    FoundWeapon = [Weapon || Weapon <- AllWeapons,
                  Weapon#weapon_type.name == Name],
    case FoundWeapon of
        [OneWeapon] -> {ok, OneWeapon};
        [] -> {error, not_found};
        E -> {error, unknown_error, E}
    end.

get_weapon_types_mock(Names, GalaxyId, State) ->
    {ok, AllWeapons} = get_weapon_types_mock(GalaxyId, State),
    FoundWeapons = [Weapon || Weapon <- AllWeapons,
                  lists:member(Weapon#weapon_type.name, Names)],
    {ok, FoundWeapons}.

get_weapon_types_mock(GalaxyId, _State) ->
    {ok, [
     #weapon_type{
        name = <<"laser_cannon">>,
        galaxy_id = GalaxyId,
        display_name = <<"Laser Cannon">>,
        strong_vs = [
                      {<<"test_patrol_ship">>, 25},
                      {<<"test_interceptor">>, 25},
                      {<<"test_bomber">>, 25}
                    ],
        weak_vs = [
                    {<<"test_corvette">>, 25},
                    {<<"test_carrier">>, 25},
                    {<<"test_destrouer">>, 25},
                    {<<"test_battle_ship">>, 25}
                  ],
        base_strength = 25,
        metadata = undefined
       },
     #weapon_type{
        name = <<"point_defense_laser">>,
        galaxy_id = GalaxyId,
        display_name = <<"Point Defense Laser">>,
        strong_vs = [
                      {<<"test_patrol_ship">>, 50},
                      {<<"test_interceptor">>, 50},
                      {<<"test_bomber">>, 50}
                    ],
        weak_vs = [
                    {<<"test_corvette">>, 50},
                    {<<"test_carrier">>, 50},
                    {<<"test_destrouer">>, 50},
                    {<<"test_battle_ship">>, 50}
                  ],
        base_strength = 50,
        metadata = undefined
       },
     #weapon_type{
        name = <<"turbo_laser">>,
        galaxy_id = GalaxyId,
        display_name = <<"Turbo Laser">>,
        strong_vs = [
                    {<<"test_corvette">>, 60},
                    {<<"test_carrier">>, 60},
                    {<<"test_destrouer">>, 60},
                    {<<"test_battle_ship">>, 60}
                  ],
        weak_vs = [
                      {<<"test_patrol_ship">>, 60},
                      {<<"test_interceptor">>, 60},
                      {<<"test_bomber">>, 60}
                    ],
        base_strength = 60,
        metadata = undefined
       },
     #weapon_type{
        name = <<"heavy_turbo_laser">>,
        galaxy_id = GalaxyId,
        display_name = <<"Heavy Turbo Laser">>,
        strong_vs = [
                    {<<"test_corvette">>, 120},
                    {<<"test_carrier">>, 120},
                    {<<"test_destrouer">>, 120},
                    {<<"test_battle_ship">>, 120}
                  ],
        weak_vs = [
                      {<<"test_patrol_ship">>, 120},
                      {<<"test_interceptor">>, 120},
                      {<<"test_bomber">>, 120}
                    ],
        base_strength = 120,
        metadata = undefined
       },
     #weapon_type{
        name = <<"torpedo_launcher">>,
        galaxy_id = GalaxyId,
        display_name = <<"Torpedo Launcher">>,
        strong_vs = [
                    {<<"test_corvette">>, 600},
                    {<<"test_carrier">>, 600},
                    {<<"test_destrouer">>, 600},
                    {<<"test_battle_ship">>, 600}
                  ],
        weak_vs = [
                      {<<"test_patrol_ship">>, 600},
                      {<<"test_interceptor">>, 600},
                      {<<"test_bomber">>, 600}
                    ],
        base_strength = 600,
        metadata = undefined
       },
      #weapon_type{
        name = <<"missile_launcher">>,
        galaxy_id = GalaxyId,
        display_name = <<"Missile Launcher">>,
        strong_vs = [
                      {<<"test_patrol_ship">>, 250},
                      {<<"test_interceptor">>, 250},
                      {<<"test_bomber">>, 250}
                    ],
        weak_vs = [
                    {<<"test_corvette">>, 250},
                    {<<"test_carrier">>, 250},
                    {<<"test_destrouer">>, 250},
                    {<<"test_battle_ship">>, 250}
                  ],
        base_strength = 250,
        metadata = undefined
       }
    ]}.


get_resource_type_mock(Name, GalaxyId) ->
    {ok, AllResources} = get_resource_types_mock(GalaxyId),
    FoundResources = [Resource || Resource <- AllResources,
                     Resource#resource_type.name == Name],
    case FoundResources of
        [] -> {error, not_found};
        [FoundResource] -> {ok, FoundResource};
        E -> {error, unknown_error, E}
    end.

get_resource_types_mock(Names, GalaxyId) ->
    {ok, AllResources} = get_resource_types_mock(GalaxyId),
    FoundResources = [Resource || Resource <- AllResources,
                     lists:member(Resource#resource_type.name, Names)],
    {ok, FoundResources}.

get_resource_types_mock(GalaxyId) ->
    {ok, [
            #resource_type{
                name = <<"test_patrol_ship">>,
                galaxy_id = GalaxyId,
                category = <<"test">>,
                storage_space = 1,
                display_name = <<"Test Patrol Ship">>,
                build_materials = [],
                build_time = 0,
                type = #force_model{
                    name = <<"test_patrol_ship">>,
                    galaxy_id = ?GALAXY_ID,
                    display_name = <<"Test Patrol Ship">>,
                    weapons = [{<<"laser_cannon">>, 2}],
                    metadata = undefined},
                metadata = undefined},
            #resource_type{
                name = <<"test_interceptor">>,
                galaxy_id = GalaxyId,
                category = <<"test">>,
                storage_space = 1,
                display_name = <<"Test Interceptor">>,
                build_materials = [],
                build_time = 0,
                type = #force_model{
                    name = <<"test_interceptor">>,
                    galaxy_id = ?GALAXY_ID,
                    display_name = <<"Test Ship">>,
                    weapons = [{<<"laser_cannon">>, 4}],
                    metadata = undefined},
                metadata = undefined},
            #resource_type{
                name = <<"test_bomber">>,
                galaxy_id = GalaxyId,
                category = <<"test">>,
                storage_space = 1,
                display_name = <<"Test Bomber">>,
                build_materials = [],
                build_time = 0,
                type = #force_model{
                    name = <<"test_interceptor">>,
                    galaxy_id = ?GALAXY_ID,
                    display_name = <<"Test Ship">>,
                    weapons = [
                               {<<"laser_cannon">>, 1},
                               {<<"torpedo_launcher">>, 1}],
                    metadata = undefined},
                metadata = undefined},
            #resource_type{
                name = <<"test_transport">>,
                galaxy_id = GalaxyId,
                category = <<"test">>,
                storage_space = 1,
                display_name = <<"Test Transport">>,
                build_materials = [],
                build_time = 0,
                type = #force_model{
                    name = <<"test_transport">>,
                    galaxy_id = ?GALAXY_ID,
                    display_name = <<"Test Transport">>,
                    weapons = [],
                    metadata = undefined},
                metadata = undefined},
            #resource_type{
                name = <<"test_corvette">>,
                galaxy_id = GalaxyId,
                category = <<"test">>,
                storage_space = 1,
                display_name = <<"Test Corvette">>,
                build_materials = [],
                build_time = 0,
                type = #force_model{
                    name = <<"test_corvette">>,
                    galaxy_id = ?GALAXY_ID,
                    display_name = <<"Test Ship">>,
                    weapons = [
                               {<<"point_defense_laser">>, 6},
                               {<<"turbo_laser">>, 2}
                              ],
                    metadata = undefined},
                metadata = undefined},
            #resource_type{
                name = <<"test_carrier">>,
                galaxy_id = GalaxyId,
                category = <<"test">>,
                storage_space = 1,
                display_name = <<"Test Carrier">>,
                build_materials = [],
                build_time = 0,
                type = #force_model{
                    name = <<"test_ship">>,
                    galaxy_id = ?GALAXY_ID,
                    display_name = <<"Test Carrier">>,
                    weapons = [{<<"turbo_laser">>, 5}],
                    metadata = undefined},
                metadata = undefined},
            #resource_type{
                name = <<"test_destroyer">>,
                galaxy_id = GalaxyId,
                category = <<"test">>,
                storage_space = 1,
                display_name = <<"Test Destroyer">>,
                build_materials = [],
                build_time = 0,
                type = #force_model{
                    name = <<"test_destroyer">>,
                    galaxy_id = ?GALAXY_ID,
                    display_name = <<"Test Destroyer">>,
                    weapons = [{<<"turbo_laser">>, 48},
                               {<<"heavy_turbo_laser">>, 8}],
                    metadata = undefined},
                metadata = undefined},
            #resource_type{
                name = <<"test_battle_ship">>,
                galaxy_id = GalaxyId,
                category = <<"test">>,
                storage_space = 1,
                display_name = <<"Test Battle Ship">>,
                build_materials = [],
                build_time = 0,
                type = #force_model{
                    name = <<"test_battle_ship">>,
                    galaxy_id = ?GALAXY_ID,
                    display_name = <<"Test Battle Ship">>,
                    weapons = [{<<"turbo_laser">>, 48},
                               {<<"heavy_turbo_laser">>, 16},
                               {<<"missile_launcher">>, 2},
                               {<<"torpedo_launcher">>, 2}],
                    metadata = undefined},
                metadata = undefined}
    ]}.

fixture_test_() ->
    {
        setup,
        fun test_setup/0, fun test_teardown/1,
            [
                {"Test calculate force capabilities",
                    ?_test(test_calculate_force_capabilities())}
            ]
    }.

test_calculate_force_capabilities() ->
    Force = #force{
        id = <<"test_force">>,
        galaxy_id = ?GALAXY_ID,
        display_name = <<"Test Force">>,
        simulation_callbacks = [],
        resources = [#resource{
                       name = <<"test_patrol_ship">>,
                       galaxy_id = ?GALAXY_ID,
                       amount = 2}],
        capabilities = [],
        strength = 0.0,
        metadata = undefined},
    
    {ok, Result} = battle_srv:calculate_force_capabilities(
                     Force,
                     mnesia_battle,
                     []),
    %% We have 2 ships in the force, each one with two weapons of
    %% 25 starfigter capability, which means we should get a total
    %% value of (2 * 25 * 2) = 100 starfighter capability.
    ?assertEqual(
        [
            {<<"test_interceptor">>, 100},
            {<<"test_bomber">>, 100},
            {<<"test_patrol_ship">>, 100}
        ],
        Result#force.capabilities).
