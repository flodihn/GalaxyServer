% Mnesia table records

-define(GALAXY_RESOURCE_EVENT_MANAGER, galaxy_resource_event_manager).

-record(galaxy, {
    id,
    pos,
    regions=[],
    metadata}).

-record(region, {
    name,
    display_name,
    galaxy_id,
    systems=[],
    metadata}).

-record(system, {
    name,
    galaxy_id,
    region,
    pos,
    display_name,
    star_type=yellow,
    star_size=100,
    planets=[],
    moons=[],
    asteroid_belts=[],
    structures=[],
    metadata}).

-record(planet, {
    name,
    galaxy_id,
    system,
    display_name,
    orbit,
    size=3,
    major_biome=desert,
    minor_biomes=[],
    temperature=cold,
    seed=0,
    resources=[],
    resource_generators=[],
    resource_converters=[],
    moons=[],
    structures=[],
    metadata}).

-record(moon, {
    name,
    planet,
    orbit,
    resources=[],
    structures=[],
    metadata}).

-record(asteroid_belt, {
    name,
    system,
    orbit,
    resources = [],
    structures = [],
    metadata}).

-record(structure_type, {
    name,
    category,
    production_rate = 1,
    produces = [],
    input_storage_space = 1000,
    output_storage_space = 1000,
    output_queue = [],
    display_name,
    metadata
    }).

-record(resource_type, {
    name,
    category,
    storage_space = 1,
    display_name,
    build_materials = [],
    build_time = 0,
    metadata}).

% Non mnesia table records

-record(resource, {
    name,
    amount}).

-record(queue_item, {
    resource,
    finish_time}).

-record(structure, {
    uid,
    name,
    build_queue = [],
    output_resources = [],
    input_resources = [],
    output_storage_space = 0,
    input_storage_space = 0
    }).

-record(orbit, {
    distance,
    angle=0,
    speed=1}).

%-define(PLANET_TYPE_FOREST, 0).
%-define(PLANET_TYPE_GIANT_FOREST, 1).
%-define(PLANET_TYPE_DESERT, 2).
%-define(PLANET_TYPE_ICE, 3).
%-define(PLANET_TYPE_WATER, 4).
%-define(PLANET_TYPE_VOLCANIC, 5).
%-define(PLANET_TYPE_BARREN, 6).
%-define(PLANET_TYPE_JUNGLE, 7).
%-define(PLANET_TYPE_EXOTIC, 8).
%-define(PLANET_TYPE_DESTROYED, 9).

%-define(DWARF_PLANET, 0).
%-define(SMALL_PLANET, 0).
%-define(MEDIUM_PLANET, 0).
%-define(LARGE_PLANET, 0).
%-define(GIANT_PLANET, 0).

%-define(STRUCTURE_TYPE_CITY, 0).
%-define(STRUCTURE_TYPE_FACTORY, 0).
%-define(STRUCTURE_TYPE_BARRACKS, 0).
%-define(STRUCTURE_TYPE_SHIPYARD, 0).
%-define(STRUCTURE_TYPE_RESARCH_FACILITY, 0).
%-define(STRUCTURE_TYPE_RESARCH_SPACE_STATION, 0).
%-define(STRUCTURE_TYPE_SPACE_STATION, 0).
%-define(STRUCTURE_TYPE_SHIELD_GENERATOR, 0).
%-define(STRUCTURE_TYPE_ION_CANNON, 0).

%-define(SMALL_STRUCTURE, 0).
%-define(MEDIUM_STRUCTURE, 0).
%-define(LARGE_STRUCTURE, 0).
%-define(SUPER_STRUCTURE, 0).

%% Raw materials
%-define(RESOURCE_TYPE_CREDITS, 0).
%-define(RESOURCE_TYPE_BACTA, 0).
%-define(RESOURCE_TYPE_FUEL, 0).
%-define(RESOURCE_TYPE_DURASTEEL, 0).
%-define(RESOURCE_TYPE_PLASTISTEEL, 0).
%-define(RESOURCE_TYPE_TRANSISTEEL, 0).
%-define(RESOURCE_TYPE_RARE_METALS, 0).

%% Finer resources
%-define(RESOURCE_TYPE_GENERAL_SUPPLIES, 0).
%-define(RESOURCE_TYPE_FOOD, 0).
%-define(RESOURCE_TYPE_WATER, 0).

%% High level resources
%-define(RESOURCE_TYPE_TURBO_LASERS, 0).
%-define(RESOURCE_TYPE_LASER_CANNONS, 0).
%-define(RESOURCE_TYPE_ION_CANNONS, 0).
%-define(RESOURCE_TYPE_STARFIGHTER_SHIELD_GENERATORS, 0).
%-define(RESOURCE_TYPE_STARFIGHTER_BEAM_WEAPONS, 0).
%-define(RESOURCE_TYPE_STARFIGHTER_COUNTERMEASURE_FLAKK, 0).
%-define(RESOURCE_TYPE_CAPITALSHIP_SHIELD_GENERATORS, 0).
%-define(RESOURCE_TYPE_CAPITALSHIP_BEAM_WEAPONS, 0).
%-define(RESOURCE_TYPE_PLANETARY_SHIELD_GENERATORS, 0).
%-define(RESOURCE_TYPE_WEAPONS, 0).

%% Human resources
%-define(RESOURCE_TYPE_SCIENTISTS, 0).
%-define(RESOURCE_TYPE_ENGINEERS, 0).

%% Illegal resources
%-define(RESOURCE_TYPE_SPICE, 0).
%-define(RESOURCE_TYPE_WANTED_CRIMINALS, 0).
%-define(RESOURCE_TYPE_STOLEN_GOODS, 0).

%% Neutral production types.
%-define(PRODUCTION_TYPE_CONCUSSION_MISSILES, 0).
%-define(PRODUCTION_TYPE_CLUSTER_MISSILES, 0).
%-define(PRODUCTION_TYPE_PROTON_TORPEDOES, 0).
%-define(PRODUCTION_TYPE_PROTON_BOMBS, 0).
%-define(PRODUCTION_TYPE_SONIC_BOMBS, 0).
%-define(PRODUCTION_TYPE_MINES, 0).
%-define(PRODUCTION_TYPE_TUG, 0).
%-define(PRODUCTION_TYPE_Z95, 0).
%-define(PRODUCTION_TYPE_YWING, 0).
%-define(PRODUCTION_TYPE_YT_1000, 0).
%-define(PRODUCTION_TYPE_YT_1200, 0).
%-define(PRODUCTION_TYPE_YT_1300, 0).
%-define(PRODUCTION_TYPE_YT_2000, 0).
%-define(PRODUCTION_TYPE_YT_2400, 0).
%-define(PRODUCTION_TYPE_FIRESPRAY, 0).
%-define(PRODUCTION_TYPE_PATROL_CRAFT, 0).
%-define(PRODUCTION_TYPE_BFF1_BULK_FREIGHTER, 0).
%-define(PRODUCTION_TYPE_CORELLIAN_CORVETTE, 0).
%-define(PRODUCTION_TYPE_CORELLIAN_CORVETTE_GUNSHIP, 0).
%-define(PRODUCTION_TYPE_QUASAR_FIRE_CRUISER, 0).
%-define(PRODUCTION_TYPE_NEBULON_B_FRIGATE, 0).
%-define(PRODUCTION_TYPE_NEBULON_B2_FRIGATE, 0).
%-define(PRODUCTION_TYPE_NEBULON_B_MEDICAL_FRIGATE, 0).
%-define(PRODUCTION_TYPE_NEBULON_B2_MEDICAL_FRIGATE, 0).

%% Imperial production types.
%-define(PRODUCTION_TYPE_TIE_FIGHTERS, 0).
%-define(PRODUCTION_TYPE_TIE_INTERCEPTORS, 0).
%-define(PRODUCTION_TYPE_TIE_BOMBERS, 0).
%-define(PRODUCTION_TYPE_TIE_ADVANCED, 0).
%-define(PRODUCTION_TYPE_TIE_DEFENDERS, 0).
%-define(PRODUCTION_TYPE_ASSAULT_GUNBOAT, 0).
%-define(PRODUCTION_TYPE_MISSILE_BOAT, 0).
%-define(PRODUCTION_TYPE_ESCORT_SHUTTLE, 0).
%-define(PRODUCTION_TYPE_GOZANTI_CRUISER, 0).
%-define(PRODUCTION_TYPE_IMPERIAL_LIGHT_CRUISER, 0).
%-define(PRODUCTION_TYPE_CARRACK_CRUISER, 0).
%-define(PRODUCTION_TYPE_IMPERIAL_CARRIER, 0).
%-define(PRODUCTION_TYPE_VICTORY_STAR_DESTROYER, 0).
%-define(PRODUCTION_TYPE_STAR_DESTROYER_I, 0).
%-define(PRODUCTION_TYPE_STAR_DESTROYER_II, 0).
%-define(PRODUCTION_TYPE_SUPER_STAR_DESTROYER, 0).
%-define(PRODUCTION_TYPE_ECLIPSE_SUPER_STAR_DESTROYER, 0).
%-define(PRODUCTION_TYPE_DEATH_STAR, 0).

%% Rebel production types.
%-define(PRODUCTION_TYPE_XWING, 0).
%-define(PRODUCTION_TYPE_AWING, 0).
%-define(PRODUCTION_TYPE_BWING, 0).
%-define(PRODUCTION_TYPE_TWING, 0).
%-define(PRODUCTION_TYPE_EWING, 0).
%-define(PRODUCTION_TYPE_REBEL_TRANSPORT, 0).
%-define(PRODUCTION_TYPE_MON_CALAMARI_CRUISER_MC40A, 0).
%-define(PRODUCTION_TYPE_MON_CALAMARI_CRUISER_MC80A, 0).
%-define(PRODUCTION_TYPE_MON_CALAMARI_CRUISER_MC80B, 0).
%-define(PRODUCTION_TYPE_MON_CALAMARI_CRUISER_COMMANDSHIP, 0).

%% Imperial ground forces
%-define(IMPERIAL_ARMY_STORMTROOPER, 0).
%-define(IMPERIAL_ARMY_SCOUTTROOPER, 0).
%-define(IMPERIAL_ARMY_SCOUTTROOPER_BIKER, 0).
%-define(IMPERIAL_ARMY_AT_AT, 0).
