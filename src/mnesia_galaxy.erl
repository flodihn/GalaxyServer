-module(mnesia_galaxy).

-define(DB_GALAXY_TABLE, galaxies).
-define(DB_RESOURCE_TYPE_TABLE, resource_types).

-include("galaxy_defs.hrl").

-export([
    init/0
    ]).

-export([
    create_galaxy/2,
    create_region/2,
    system_exists/3,
    create_system/2,
    create_planet/2,
    list_systems/2
    ]).

init() ->
    mnesia:start(),
    GalaxyAttributes = record_info(fields, galaxy),
    create_table(?DB_GALAXY_TABLE, galaxy, GalaxyAttributes, []),
    ResourceTypeAttributes = record_info(fields, resource_type),
    create_table(?DB_RESOURCE_TYPE_TABLE, resource_type,
        ResourceTypeAttributes, []),
   {ok, []}.

create_galaxy_tables(GalaxyId) ->
    RegionsTable = get_regions_table(GalaxyId),
    RegionAttributes = record_info(fields, region),
    create_table(RegionsTable, region, RegionAttributes, [galaxy_id]),

    SystemsTable = get_systems_table(GalaxyId),
    SystemAttributes = record_info(fields, system),
    create_table(SystemsTable, system, SystemAttributes, [galaxy_id]),

    PlanetsTable = get_planets_table(GalaxyId),
    PlanetsAttributes = record_info(fields, planet),
    create_table(PlanetsTable, planet, PlanetsAttributes, [galaxy_id]).
 
create_table(TableName, RecordName, Attributes, IndexList) ->
    case lists:member(TableName, mnesia:system_info(tables)) of
        true ->
            {ok, already_exists};
        false ->
            change_to_disc_schema(),
            mnesia:create_table(TableName, 
                [
                    {record_name, RecordName},
                    {disc_copies, [node()]},
                    {type, set},
                    {attributes, Attributes}
                ]),
            ok = create_indexes(TableName, IndexList),
            {ok, created}
    end.

create_indexes(_TableName, []) ->
    ok;

create_indexes(TableName, [Field | Rest]) ->
    mnesia:add_table_index(TableName, Field),
    create_indexes(TableName, Rest).

change_to_disc_schema() ->
    mnesia:change_config(extra_db_nodes, [node()]),
    mnesia:change_table_copy_type(schema, node(), disc_copies).

system_exists(GalaxyId, SystemName, _State) ->
    SystemsTable = get_systems_table(GalaxyId),
    case mnesia:dirty_read(SystemsTable, SystemName) of
        [_Record] -> true;
        [] -> false
    end.

create_galaxy(Galaxy, _State) -> 
    io:format("Creating galaxy: ~p.~n", [Galaxy#galaxy.id]),
    create_galaxy_tables(Galaxy#galaxy.id),
    WriteGalaxy = fun() ->
        mnesia:write(?DB_GALAXY_TABLE, Galaxy, write)
    end,
    case mnesia:transaction(WriteGalaxy) of
        {atomic, ok} ->
            {ok, galaxy_created};
        {aborted, Reason} ->
            {error, Reason}
    end.

create_region(Region = #region{}, _State) ->
    GalaxyId = Region#region.galaxy_id,
    RegionTable = get_regions_table(GalaxyId),
    WriteRegion = fun() ->
        [Galaxy] = mnesia:read(?DB_GALAXY_TABLE, GalaxyId, write),
        mnesia:write(?DB_GALAXY_TABLE, Galaxy#galaxy{
            regions=lists:append(
                Galaxy#galaxy.regions, [Region#region.name])},
            write),
        mnesia:write(RegionTable, Region, write)
    end,
    case mnesia:transaction(WriteRegion) of
        {atomic, ok} ->
            {ok, region_created};
        {aborted, Reason} ->
            {error, Reason}
    end;

create_region(_Region, _State) ->
    {error, bad_region_record}.

create_system(System = #system{}, _State) ->
    SystemsTable = get_systems_table(System#system.galaxy_id),
    RegionsTable = get_regions_table(System#system.galaxy_id),
    WriteSystem = fun() ->
        [Region] = mnesia:read(RegionsTable, System#system.region),
        mnesia:write(SystemsTable, System, write),
        mnesia:write(RegionsTable, Region#region{systems=lists:append(
            Region#region.systems, [System#system.name])}, write)
    end,
    case mnesia:transaction(WriteSystem) of
        {atomic, ok} ->
            {ok, system_created};
        {aborted, Reason} ->
            {error, Reason}
    end;


create_system(_System, _State) ->
    {error, bad_system_record}.

create_planet(Planet = #planet{}, _State) ->
    SystemsTable = get_systems_table(Planet#planet.galaxy_id),
    PlanetsTable = get_planets_table(Planet#planet.galaxy_id),
    WritePlanet = fun() ->
        [System] = mnesia:read(SystemsTable, Planet#planet.system),
        mnesia:write(PlanetsTable, Planet, write),
        mnesia:write(SystemsTable, System#system{planets=lists:append(
            System#system.planets, [Planet#planet.name])}, write)
    end,
    case mnesia:transaction(WritePlanet) of
        {atomic, ok} ->
            {ok, planet_created};
        {aborted, Reason} ->
            {error, Reason}
    end;

create_planet(_Planet, _State) ->
    {error, bad_planet_record}.

list_systems(GalaxyId, _State) ->
    SystemsTable = get_systems_table(GalaxyId),
    Iterator = fun(_Record, _) -> [] end,
    ReadAll = fun() ->
        mnesia:foldl(Iterator, [], SystemsTable)
    end,
    case mnesia:transaction(ReadAll) of
        {atomic, AllSystems} ->
            {ok, AllSystems};
        {aborted, Reason} ->
            {error, Reason}
    end.

get_regions_table(GalaxyId) ->
    list_to_atom(binary_to_list(GalaxyId) ++ "_regions").

get_systems_table(GalaxyId) ->
    list_to_atom(binary_to_list(GalaxyId) ++ "_systems").

get_planets_table(GalaxyId) ->
    list_to_atom(binary_to_list(GalaxyId) ++ "_planets").
