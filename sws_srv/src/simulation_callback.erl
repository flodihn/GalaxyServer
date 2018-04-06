-module(simulation_callback).

-export([
    simulate_system/2
    ]).

-include("galaxy_defs.hrl").
-include("resource_defs.hrl").

simulate_system(System, DeltaTime) ->
    GalaxyId = System#system.galaxy_id,
    Planets = System#system.planets,
    simulate_planets(Planets, GalaxyId, DeltaTime).

simulate_planets([], _GalaxyId, _DeltaTime) ->
    done;

simulate_planets([PlanetName | Rest], GalaxyId, DeltaTime) ->
    simulate_planet(GalaxyId, PlanetName, DeltaTime),
    simulate_planets(Rest, GalaxyId, DeltaTime).

simulate_planet(GalaxyId, PlanetName, DeltaTime) ->
    {ok, Planet} = galaxy_srv:get_planet(GalaxyId, PlanetName),
    Structures = Planet#planet.structures,
    {ok, UpdatedStructures} = galaxy_structure_util:simulate_structures(
        Structures, DeltaTime),
    UpdatedPlanet = Planet#planet{structures = UpdatedStructures},
    {ok, planet_updated} = galaxy_srv:update_planet(UpdatedPlanet).
