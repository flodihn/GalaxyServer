-module(battle_srv_tests).

-include_lib("eunit/include/eunit.hrl").

-include("battle_defs.hrl").
-include("../resource_srv/include/resource_defs.hrl").
-include("../galaxy_srv/include/galaxy_defs.hrl").

-define(GALAXY_ID, <<"battle_srv_test">>).
-define(EXISTING_SYSTEM, <<"test_system">>).
-define(NON_EXISTING_SYSTEM, <<"test_non_system">>).


test_setup() ->
    meck:new(mnesia_battle, []),
    meck:new(resource_srv, [non_strict]),
    meck:new(galaxy_srv, [non_strict]),
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

    meck:expect(mnesia_battle, get_force_model,
                fun get_force_model_mock/3),
    meck:expect(mnesia_battle, get_force_models,
                fun get_force_models_mock/2),

    meck:expect(mnesia_battle, create_force,
                fun create_force_mock/2),
    meck:expect(mnesia_battle, get_force,
                fun get_force_mock/3),
    meck:expect(mnesia_battle, update_force,
                fun update_force_mock/2),

    meck:expect(galaxy_srv, get_system,
                fun get_system_mock/2),
    meck:expect(galaxy_srv, add_force_to_system,
                fun add_force_to_system_mock/3),
    meck:expect(galaxy_srv, remove_force_from_system,
                fun remove_force_from_system_mock/3),
    ok.

test_teardown(_Arg) ->
    meck:unload(mnesia_battle),
    meck:unload(resource_srv),
    ok.

get_system_mock(GalaxyId, SystemName) ->
    case SystemName of
        ?EXISTING_SYSTEM ->
            {ok, #system{
                    name = ?EXISTING_SYSTEM,
                    galaxy_id = <<"test_region">>,
                    pos = {0, 0, 0},
                    display_name = <<"Some System">>,
                    star_type = medium_white,
                    star_size = 1,
                    planets = [],
                    moons = [],
                    asteroid_belts = [],
                    structures = [],
                    routes = [],
                    forces = [],
                    metadata = undefined}};
        ?NON_EXISTING_SYSTEM ->
            {error, not_found}
    end.

add_force_to_system_mock(_GalaxyId, _ForceId, _SystemName) ->
    {ok, force_added}.

remove_force_from_system_mock(_GalaxyId, _ForceId, _SystemName) ->
    {ok, force_removed}.

update_force_mock(Force, _State) ->
    put(Force#force.id, Force),
    {ok, force_updated}.

create_force_mock(Force, _State) ->
    ForceId = Force#force.id,
    put(ForceId, Force),
    {ok, Force}.

get_force_mock(ForceId, _GalaxyId, _State) ->
    Force = get(ForceId),
    {ok, Force}.

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
        modifiers = [
            {<<"test_patrol_ship">>, 25},
            {<<"test_interceptor">>, 25},
            {<<"test_bomber">>, 25},
            {<<"test_corvette">>, -25},
            {<<"test_carrier">>, -25},
            {<<"test_destroyer">>, -25},
            {<<"test_battle_ship">>, -25}
        ],
        base_strength = 25,
        metadata = undefined
       },
     #weapon_type{
        name = <<"point_defense_laser">>,
        galaxy_id = GalaxyId,
        display_name = <<"Point Defense Laser">>,
        modifiers = [
            {<<"test_patrol_ship">>, 50},
            {<<"test_interceptor">>, 50},
            {<<"test_bomber">>, 50},
            {<<"test_corvette">>, -50},
            {<<"test_carrier">>, -50},
            {<<"test_destroyer">>, -50},
            {<<"test_battle_ship">>, -50}
        ],
        base_strength = 50,
        metadata = undefined
       },
     #weapon_type{
        name = <<"turbo_laser">>,
        galaxy_id = GalaxyId,
        display_name = <<"Turbo Laser">>,
        modifiers = [
            {<<"test_corvette">>, 60},
            {<<"test_carrier">>, 60},
            {<<"test_destroyer">>, 60},
            {<<"test_battle_ship">>, 60},
            {<<"test_patrol_ship">>, -60},
            {<<"test_interceptor">>, -60},
            {<<"test_bomber">>, -60}
        ],
        base_strength = 60,
        metadata = undefined
       },
     #weapon_type{
        name = <<"heavy_turbo_laser">>,
        galaxy_id = GalaxyId,
        display_name = <<"Heavy Turbo Laser">>,
        modifiers = [
            {<<"test_corvette">>, 120},
            {<<"test_carrier">>, 120},
            {<<"test_destroyer">>, 120},
            {<<"test_battle_ship">>, 120},
            {<<"test_patrol_ship">>, -120},
            {<<"test_interceptor">>, -120},
            {<<"test_bomber">>, -120}
        ],
        base_strength = 120,
        metadata = undefined
       },
     #weapon_type{
        name = <<"torpedo_launcher">>,
        galaxy_id = GalaxyId,
        display_name = <<"Torpedo Launcher">>,
        modifiers = [
            {<<"test_corvette">>, 600},
            {<<"test_carrier">>, 600},
            {<<"test_destroyer">>, 600},
            {<<"test_battle_ship">>, 600},
            {<<"test_patrol_ship">>, -600},
            {<<"test_interceptor">>, -600},
            {<<"test_bomber">>, -600}
        ],
        base_strength = 600,
        metadata = undefined
       },
      #weapon_type{
        name = <<"missile_launcher">>,
        galaxy_id = GalaxyId,
        display_name = <<"Missile Launcher">>,
        modifiers = [
            {<<"test_patrol_ship">>, 250},
            {<<"test_interceptor">>, 250},
            {<<"test_bomber">>, 250},
            {<<"test_corvette">>, -250},
            {<<"test_carrier">>, -250},
            {<<"test_destroyer">>, -250},
            {<<"test_battle_ship">>, -250}
        ],
        base_strength = 250,
        metadata = undefined
       }
    ]}.

gen_server_call_mock(Server, Arg) ->
    case Arg of 
        {Fun, Data} ->
            apply(Server, Fun, Data);
        {Fun} ->
            apply(Server, Fun, []);
        _Other ->
            {error, bad_server_mock_arg, Arg}
    end.

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
                type = #force_model_name{
                    name = <<"test_patrol_ship">>},
                metadata = undefined},
            #resource_type{
                name = <<"test_interceptor">>,
                galaxy_id = GalaxyId,
                category = <<"test">>,
                storage_space = 1,
                display_name = <<"Test Interceptor">>,
                build_materials = [],
                build_time = 0,
                type = #force_model_name{
                    name = <<"test_interceptor">>},
                metadata = undefined},
            #resource_type{
                name = <<"test_bomber">>,
                galaxy_id = GalaxyId,
                category = <<"test">>,
                storage_space = 1,
                display_name = <<"Test Bomber">>,
                build_materials = [],
                build_time = 0,
                type = #force_model_name{
                    name = <<"test_bomber">>},
                metadata = undefined},
            #resource_type{
                name = <<"test_transport">>,
                galaxy_id = GalaxyId,
                category = <<"test">>,
                storage_space = 1,
                display_name = <<"Test Transport">>,
                build_materials = [],
                build_time = 0,
                type = #force_model_name{
                    name = <<"test_transport">>},
                metadata = undefined},
            #resource_type{
                name = <<"test_corvette">>,
                galaxy_id = GalaxyId,
                category = <<"test">>,
                storage_space = 1,
                display_name = <<"Test Corvette">>,
                build_materials = [],
                build_time = 0,
                type = #force_model_name{
                    name = <<"test_corvette">>},
                metadata = undefined},
            #resource_type{
                name = <<"test_carrier">>,
                galaxy_id = GalaxyId,
                category = <<"test">>,
                storage_space = 1,
                display_name = <<"Test Carrier">>,
                build_materials = [],
                build_time = 0,
                type = #force_model_name{
                    name = <<"test_ship">>},
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
                    name = <<"test_destroyer">>},
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
                    name = <<"test_battle_ship">>},
                metadata = undefined}
    ]}.

get_force_model_mock(Name, GalaxyId, State) ->
    {ok, AllModels} = get_force_models_mock(GalaxyId, State),
    FoundModels = [Model || Model <- AllModels,
                            Model#force_model.name == Name],
    {ok, lists:nth(1, FoundModels)}.

get_force_models_mock(GalaxyId, State) ->
    {ok, [
        #force_model{
            name = <<"test_patrol_ship">>,
            galaxy_id = ?GALAXY_ID,
            display_name = <<"Test Patrol Ship">>,
            weapons = [{<<"laser_cannon">>, 2}],
            metadata = undefined},
        #force_model{
            name = <<"test_interceptor">>,
            galaxy_id = ?GALAXY_ID,
            display_name = <<"Test Interceptor">>,
            weapons = [{<<"laser_cannon">>, 4}],
            metadata = undefined},
        #force_model{
            name = <<"test_bomber">>,
            galaxy_id = ?GALAXY_ID,
            display_name = <<"Test Bomber">>,
            weapons = [
                {<<"torpedo_launcher">>, 2}],
            metadata = undefined},
        #force_model{
            name = <<"test_corvette">>,
            galaxy_id = ?GALAXY_ID,
            display_name = <<"Test Ship">>,
            weapons = [
                {<<"point_defense_laser">>, 6},
                {<<"turbo_laser">>, 2}
                ],
            metadata = undefined},
        #force_model{
            name = <<"test_ship">>,
            galaxy_id = ?GALAXY_ID,
            display_name = <<"Test Carrier">>,
            weapons = [{<<"turbo_laser">>, 5}],
            metadata = undefined},
        #force_model{
            name = <<"test_destroyer">>,
            galaxy_id = ?GALAXY_ID,
            display_name = <<"Test Destroyer">>,
            weapons = [{<<"turbo_laser">>, 48},
                {<<"heavy_turbo_laser">>, 8}],
            metadata = undefined},
        #force_model{
            name = <<"test_battle_ship">>,
            galaxy_id = ?GALAXY_ID,
            display_name = <<"Test Battle Ship">>,
            weapons = [{<<"turbo_laser">>, 48},
                {<<"heavy_turbo_laser">>, 16},
                {<<"missile_launcher">>, 2},
                {<<"torpedo_launcher">>, 2}],
            metadata = undefined}
    ]}.

fixture_test_() ->
    {setup,
    fun test_setup/0, fun test_teardown/1, [
        {"Test calculate force capabilities",
        ?_test(test_calculate_force_capabilities())},
        {"Test add resources to force",
        ?_test(test_add_resources_to_force())},
        {"Test force capabilities recalculated after add resources",
        ?_test(test_force_capabilties_recalculated_after_add_resources())},
        {"Test move force to system",
        ?_test(test_move_force_to_system())},
        {"Test move force to non existing system",
        ?_test(test_move_force_to_non_existing_system())},
        {"Test system changed when force moved",
        ?_test(test_system_changed_when_force_moved())}
    ]}.

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
            {<<"test_carrier">>, -100},
            {<<"test_destroyer">>, -100},
            {<<"test_interceptor">>, 100},
            {<<"test_battle_ship">>, -100},
            {<<"test_corvette">>, -100},
            {<<"test_bomber">>, 100},
            {<<"test_patrol_ship">>, 100}
        ],
        Result#force.capabilities).

test_add_resources_to_force() ->
    ForceId = <<"test_force">>,
    Force = #force{
        id = ForceId,
        galaxy_id = ?GALAXY_ID,
        display_name = <<"Test Force">>,
        simulation_callbacks = [],
        resources = [],
        capabilities = [],
        strength = 0.0,
        metadata = undefined},
    
    mnesia_battle:create_force(Force, []),

    Resources = [
        #resource{
            name = <<"test_patrol_ship">>,
            galaxy_id = ?GALAXY_ID,
            amount = 2}
    ],
    
    {reply, {ok, added}, _State} = battle_srv:handle_call(
        {add_resources_to_force, ForceId, ?GALAXY_ID, Resources},
        undefined,
        {state, mnesia_battle, []}),

    {ok, Result} = mnesia_battle:get_force(ForceId, ?GALAXY_ID, []),
    %% We have 2 ships in the force, each one with two weapons of
    %% 25 starfigter capability, which means we should get a total
    %% value of (2 * 25 * 2) = 100 starfighter capability.
    %?assertEqual(
    %    [
    %        {<<"test_interceptor">>, 100},
    %        {<<"test_bomber">>, 100},
    %        {<<"test_patrol_ship">>, 100}
    %    ],
    %    Result#force.capabilities).
    ?assertEqual(
        Resources,
        Result#force.resources).

test_force_capabilties_recalculated_after_add_resources() ->
    ForceId = <<"test_force">>,
    Force = #force{
        id = ForceId,
        galaxy_id = ?GALAXY_ID,
        display_name = <<"Test Force">>,
        simulation_callbacks = [],
        resources = [],
        capabilities = [],
        strength = 0.0,
        metadata = undefined},
    
    mnesia_battle:create_force(Force, []),

    Resources = [
        #resource{
            name = <<"test_bomber">>,
            galaxy_id = ?GALAXY_ID,
            amount = 1}
    ],
    
    {reply, {ok, added}, _State} = battle_srv:handle_call(
        {add_resources_to_force, ForceId, ?GALAXY_ID, Resources},
        undefined,
        {state, mnesia_battle, []}),

    {ok, Result} = mnesia_battle:get_force(ForceId, ?GALAXY_ID, []),
    %% We have 1 ship in the force, each one with two weapons of
    %% 600 capability against all large ships (carriers, destroyer,
    %% battle ship and corvette), which means we should get a total
    %% value of (1 * 600 * 2) = 1200 capability.
    ?assertEqual(
        [
            {<<"test_carrier">>, 1200},
            {<<"test_interceptor">>, -1200},
            {<<"test_destroyer">>, 1200},
            {<<"test_battle_ship">>, 1200},
            {<<"test_corvette">>, 1200},
            {<<"test_bomber">>, -1200},
            {<<"test_patrol_ship">>, -1200}
        ],
        Result#force.capabilities).

test_move_force_to_system() ->
    ForceId = <<"test_force">>,
    Force = #force{
        id = ForceId,
        galaxy_id = ?GALAXY_ID,
        display_name = <<"Test Force">>,
        simulation_callbacks = [],
        resources = [],
        capabilities = [],
        strength = 0.0,
        location = undefined,
        metadata = undefined},
    
    mnesia_battle:create_force(Force, []),
    
    {ok, UpdatedForce} = battle_srv:move_force_to_system(
        Force, ?EXISTING_SYSTEM),

    ?assertEqual(?EXISTING_SYSTEM, UpdatedForce#force.location).

test_move_force_to_non_existing_system() ->
    ForceId = <<"test_force">>,
    Force = #force{
        id = ForceId,
        galaxy_id = ?GALAXY_ID,
        display_name = <<"Test Force">>,
        simulation_callbacks = [],
        resources = [],
        capabilities = [],
        strength = 0.0,
        location = undefined,
        metadata = undefined},
    
    mnesia_battle:create_force(Force, []),
    
    ?assertEqual(
       {error, system_not_found},
       battle_srv:move_force_to_system(Force, ?NON_EXISTING_SYSTEM)).

test_system_changed_when_force_moved() ->
    SystemToBeChanged = <<"system_to_be_changed">>,
    ForceId = <<"test_force">>,
    Force = #force{
        id = ForceId,
        galaxy_id = ?GALAXY_ID,
        display_name = <<"Test Force">>,
        simulation_callbacks = [],
        resources = [],
        capabilities = [],
        strength = 0.0,
        location = SystemToBeChanged,
        metadata = undefined},
    
    mnesia_battle:create_force(Force, []),
    
    {ok, UpdatedForce} = battle_srv:move_force_to_system(
                           Force, ?EXISTING_SYSTEM),

    ?assertNotEqual(SystemToBeChanged, UpdatedForce#force.location).


