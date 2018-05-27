-module(mnesia_galaxy).

-define(DB_GALAXY_TABLE, galaxies).
-define(DB_RESOURCE_TYPE_TABLE, resource_types).
-define(DB_STRUCTURE_TYPE_TABLE, structure_types).

-include("galaxy_defs.hrl").
-include("resource_defs.hrl").

-export([
    init/0
    ]).

-export([
    create_galaxy/2,
    update_galaxy/2,
	destroy_galaxy/2,
    get_galaxies/1,
	get_galaxy/2,
    create_region/2,
    get_regions/2,
    get_region/3,
    add_region_to_galaxy_record/3,
    system_exists/3,
    create_system/2,
    get_systems/2,
    get_system/3,
	remove_system/3,
	connect_system/4,
	disconnect_system/4,
    create_planet/2,
    get_planet/3,
    update_planet/2,
    create_resource_type/2,
    create_structure_type/2,
    get_resource_type/2,
    get_structure_type/2,
    get_resource/5,
    add_resource/5,
    add_structure/5,
	consistency_fix/1,
	create_galaxy_tables/1
    ]).

init() ->
    mnesia:start(),
    GalaxyAttributes = record_info(fields, galaxy),
    create_table(?DB_GALAXY_TABLE, galaxy, GalaxyAttributes, [], set),
    
    ResourceTypeAttributes = record_info(fields, resource_type),
    create_table(?DB_RESOURCE_TYPE_TABLE, resource_type,
        ResourceTypeAttributes, [category], set),

    StructureTypeAttributes = record_info(fields, structure_type),
    create_table(?DB_STRUCTURE_TYPE_TABLE, structure_type,
        StructureTypeAttributes, [category], set),
 
    {ok, []}.

create_galaxy_tables(GalaxyId) ->
    RegionsTable = get_regions_table(GalaxyId),
    RegionAttributes = record_info(fields, region),
    create_table(RegionsTable, region, RegionAttributes, [galaxy_id], set),

    SystemsTable = get_systems_table(GalaxyId),
    SystemAttributes = record_info(fields, system),
    create_table(SystemsTable, system, SystemAttributes, [galaxy_id], set),

    PlanetsTable = get_planets_table(GalaxyId),
    PlanetsAttributes = record_info(fields, planet),
    create_table(PlanetsTable, planet, PlanetsAttributes, [galaxy_id], set).

destroy_galaxy_tables(GalaxyId) ->
    RegionsTable = get_regions_table(GalaxyId),
    mnesia:delete_table(RegionsTable),

    SystemsTable = get_systems_table(GalaxyId),
    mnesia:delete_table(SystemsTable),

    PlanetsTable = get_planets_table(GalaxyId),
    mnesia:delete_table(PlanetsTable).

create_table(TableName, RecordName, Attributes, IndexList, Type) ->
    case lists:member(TableName, mnesia:system_info(tables)) of
        true ->
            {ok, already_exists};
        false ->
            change_to_disc_schema(),
            mnesia:create_table(TableName, 
                [
                    {record_name, RecordName},
                    {disc_copies, [node()]},
                    {type, Type},
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

consistency_fix(GalaxyId) ->
    RegionsTable = get_regions_table(GalaxyId),
	{ok, Regions} = read_all_records(RegionsTable),
	Results = [region_consistency_fix(GalaxyId, Region) || Region <- Regions],
	{ok, Results}.

region_consistency_fix(GalaxyId, Region) ->
	SystemNames = Region#region.systems,
	purge_non_existing_systems_from_region(GalaxyId, Region, SystemNames).

purge_non_existing_systems_from_region(GalaxyId, PossibleUpdatedRegion, []) ->
	RegionsTable = get_regions_table(GalaxyId),
	T = fun() ->
		mnesia:write(RegionsTable, PossibleUpdatedRegion, write)
	end,
	case mnesia:transaction(T) of
		{atomic, ok} -> {consistency_fixed_on_region, PossibleUpdatedRegion#region.name};
		Error -> {consistency_failed_on_region, PossibleUpdatedRegion#region.name, Error}
	end;
	
purge_non_existing_systems_from_region(GalaxyId, Region, [SystemName | SystemsNames]) ->
	case system_exists(GalaxyId, SystemName) of
		true -> 
			pass;
		false ->
			SystemList = Region#region.systems,
			UpdatedSystemList = lists:delete(SystemName, SystemList),
			UpdatedRegion = Region#region{systems = UpdatedSystemList},
			purge_non_existing_systems_from_region(GalaxyId, UpdatedRegion, SystemsNames)
	end.


read_galaxy(GalaxyId) ->
	T = fun() ->
        mnesia:read(?DB_GALAXY_TABLE, GalaxyId)
    end,
    case mnesia:transaction(T) of
        {atomic, [Galaxy]} ->
            {ok, Galaxy};
		{atomic, []} ->
            {error, not_found};
        {aborted, Reason} ->
            {error, Reason}
    end.

create_galaxy(Galaxy, _State) -> 
    create_galaxy_tables(Galaxy#galaxy.id),
    T = fun() ->
        mnesia:write(?DB_GALAXY_TABLE, Galaxy, write)
    end,
    case mnesia:transaction(T) of
        {atomic, ok} ->
            {ok, galaxy_created};
        {aborted, Reason} ->
            {error, Reason}
    end.

update_galaxy(Galaxy, _State) -> 
    T = fun() ->
        mnesia:write(?DB_GALAXY_TABLE, Galaxy, write)
    end,
    case mnesia:transaction(T) of
        {atomic, ok} ->
            {ok, galaxy_updated};
        {aborted, Reason} ->
            {error, Reason}
    end.

destroy_galaxy(GalaxyId, _State) ->
	destroy_galaxy_tables(GalaxyId),
    T = fun() ->
        mnesia:delete(?DB_GALAXY_TABLE, GalaxyId, write)
    end,
    case mnesia:transaction(T) of
        {atomic, ok} ->
            {ok, galaxy_destroyed};
        {aborted, Reason} ->
            {error, Reason}
    end.

get_galaxy(GalaxyId, _State) ->
	read_galaxy(GalaxyId).

get_galaxies(_State) ->
    Iterator = fun(Record, Acc) -> lists:append(Acc, [Record]) end,
    T = fun() ->
        mnesia:foldl(Iterator, [], ?DB_GALAXY_TABLE)
    end,
    case mnesia:transaction(T) of
        {atomic, AllGalaxies} ->
            {ok, AllGalaxies};
        {aborted, Reason} ->
            {error, Reason}
    end.

create_region(Region = #region{}, _State) ->
    GalaxyId = Region#region.galaxy_id,
    RegionTable = get_regions_table(GalaxyId),
    T = fun() ->
        mnesia:write(RegionTable, Region, write)
    end,
    case mnesia:transaction(T) of
        {atomic, ok} ->
            {ok, region_created};
        {aborted, Reason} ->
            {error, Reason}
    end;

create_region(_Region, _State) ->
    {error, bad_region_record}.

add_region_to_galaxy_record(GalaxyId, RegionName, State) ->
    T = fun() ->
        [Galaxy] = mnesia:read(?DB_GALAXY_TABLE, GalaxyId, read),
        case lists:member(RegionName, Galaxy#galaxy.regions) of
        	true ->
            	pass;
            false ->
            	MergedRegions = lists:merge(Galaxy#galaxy.regions, [RegionName]),
                mnesia:write(?DB_GALAXY_TABLE, Galaxy#galaxy{regions=MergedRegions}, write)
            end
     end,
     case mnesia:transaction(T) of
        {atomic, ok} ->
            {ok, region_added};
        {aborted, Reason} ->
            {error, Reason}
    end.

get_regions(GalaxyId, _State) ->
    RegionsTable = get_regions_table(GalaxyId),
    read_all_records(RegionsTable).

get_region(GalaxyId, RegionName, _State) ->
    RegionsTable = get_regions_table(GalaxyId),
    T = fun() ->
        mnesia:read(RegionsTable, RegionName)
    end,
    case mnesia:transaction(T) of
        {atomic, []} ->
            {error, region_not_found};
        {atomic, [Region]} ->
            {ok, Region};
        {aborted, Reason} ->
            {error, Reason}
    end.

create_system(System = #system{}, _State) ->
    SystemsTable = get_systems_table(System#system.galaxy_id),
    RegionsTable = get_regions_table(System#system.galaxy_id),
    T = fun() ->
        [Region] = mnesia:read(RegionsTable, System#system.region),
        mnesia:write(SystemsTable, System, write),
		
		
        mnesia:write(RegionsTable, Region#region{systems=lists:append(
            Region#region.systems, [System#system.name])}, write)
    end,
    case mnesia:transaction(T) of
        {atomic, ok} ->
            {ok, system_created};
        {aborted, Reason} ->
            {error, Reason}
    end;

create_system(_System, _State) ->
    {error, bad_system_record}.

remove_system(GalaxyId, SystemName, _State) ->
	SystemsTable = get_systems_table(GalaxyId),
	RegionsTable = get_regions_table(GalaxyId),
	T = fun() ->
		[System] = mnesia:read(SystemsTable, SystemName, read),	
		[Region] = mnesia:read(RegionsTable, System#system.region),
        mnesia:delete(SystemsTable, SystemName, write),
		RegionSystems = Region#region.systems,
		UpdatedRegionSystems = lists:delete(System#system.name, RegionSystems),
        mnesia:write(RegionsTable, Region#region{systems=UpdatedRegionSystems}, write)
    end,
    case mnesia:transaction(T) of
        {atomic, ok} ->
            {ok, system_removed};
        {aborted, Reason} ->
            {error, Reason}
    end.

get_systems(GalaxyId, _State) ->
    SystemsTable = get_systems_table(GalaxyId),
    read_all_records(SystemsTable).

get_system(GalaxyId, SystemName, _State) ->
    SystemsTable = get_systems_table(GalaxyId),
    T = fun() ->
        mnesia:read(SystemsTable, SystemName)
    end,
    case mnesia:transaction(T) of
        {atomic, [System]} ->
            {ok, System};
        {aborted, _Reason} ->
            {error, system_not_found}
    end.

system_exists(GalaxyId, SystemName) ->
    system_exists(GalaxyId, SystemName, undefined).

system_exists(GalaxyId, SystemName, _State) ->
    SystemsTable = get_systems_table(GalaxyId),
    case mnesia:dirty_read(SystemsTable, SystemName) of
        [_Record] -> true;
        [] -> false
    end.

connect_system(GalaxyId, OriginSystem, DestinationSystem, _State) ->
	SystemsTable = get_systems_table(GalaxyId),
	T = fun() ->
        [System] = mnesia:read(SystemsTable, OriginSystem),
		case lists:member(DestinationSystem, System#system.routes) of
			true ->
				ok;
			false ->
				mnesia:write(SystemsTable, System#system{routes=lists:append(
            		System#system.routes, [DestinationSystem])}, write)
		end
	end,
	case mnesia:transaction(T) of
		{atomic, ok} ->
			{ok, system_connected};
		{aborted, Reason} ->
			{error, Reason}
	end.

disconnect_system(GalaxyId, OriginSystem, DestinationSystem, _State) ->
	SystemsTable = get_systems_table(GalaxyId),
	T = fun() ->
        [System] = mnesia:read(SystemsTable, OriginSystem),
		case lists:member(DestinationSystem, System#system.routes) of
			true ->
				mnesia:write(SystemsTable, System#system{routes=lists:delete(
            		DestinationSystem, System#system.routes)}, write);
			false ->
				ok
		end
	end,
	case mnesia:transaction(T) of
		{atomic, ok} ->
			{ok, system_disconnected};
		{aborted, Reason} ->
			{error, Reason}
	end.

create_planet(Planet = #planet{}, _State) ->
    SystemsTable = get_systems_table(Planet#planet.galaxy_id),
    PlanetsTable = get_planets_table(Planet#planet.galaxy_id),
    T = fun() ->
        [System] = mnesia:read(SystemsTable, Planet#planet.system),
        mnesia:write(PlanetsTable, Planet, write),
        mnesia:write(SystemsTable, System#system{planets=lists:append(
            System#system.planets, [Planet#planet.name])}, write)
    end,
    case mnesia:transaction(T) of
        {atomic, ok} ->
            {ok, planet_created};
        {aborted, Reason} ->
            {error, Reason}
    end.

update_planet(Planet = #planet{}, _State) ->
    PlanetsTable = get_planets_table(Planet#planet.galaxy_id),
    T = fun() ->
        mnesia:write(PlanetsTable, Planet, write)
    end,
    case mnesia:transaction(T) of
        {atomic, ok} ->
            {ok, planet_updated};
        {aborted, Reason} ->
            {error, Reason}
    end.

get_planet(GalaxyId, PlanetName, _State) ->
    PlanetsTable = get_planets_table(GalaxyId),
    T = fun() ->
        mnesia:read(PlanetsTable, PlanetName)
    end,
    case mnesia:transaction(T) of
        {atomic, [Planet]} ->
            {ok, Planet};
        {aborted, _Reason} ->
            {error, planet_not_found}
    end.

create_resource_type(ResourceType, _State) ->
    T = fun() ->
        mnesia:write(?DB_RESOURCE_TYPE_TABLE, ResourceType, write)
    end,
    case mnesia:transaction(T) of
        {atomic, ok} ->
            {ok, resource_type_created};
        {aborted, Reason} ->
            {error, Reason}
    end.

get_resource_type(ResourceName, _State) ->
    T = fun() ->
        mnesia:read(?DB_RESOURCE_TYPE_TABLE, ResourceName)
    end,
    case mnesia:transaction(T) of
        {atomic, [ResourceType]} ->
            {ok, ResourceType};
        {atomic, []} ->
            {error, not_found};
        {aborted, Reason} ->
            {error, Reason}
    end.

create_structure_type(StructureType, _State) ->
    T = fun() ->
        mnesia:write(?DB_STRUCTURE_TYPE_TABLE, StructureType, write)
    end,
    case mnesia:transaction(T) of
        {atomic, ok} ->
            {ok, structure_type_created};
        {aborted, Reason} ->
            {error, Reason}
    end.

get_structure_type(StructureName, _State) ->
    T = fun() ->
        mnesia:read(?DB_STRUCTURE_TYPE_TABLE, StructureName)
    end,
    case mnesia:transaction(T) of
        {atomic, [StructureType]} ->
            {ok, StructureType};
        {aborted, _Reason} ->
            {error, planet_not_found}
    end.

get_resource(GalaxyId, LinkId, planet, Resource, _State) ->
    PlanetsTable = get_planets_table(GalaxyId),
    T = fun() ->
        mnesia:read(PlanetsTable, LinkId)
    end,
    case mnesia:transaction(T) of
        {atomic, [Planet]} ->
            case lists:keysearch(Resource#resource.name, 1,
                    Planet#planet.resources) of
                {ok, {_ResourceName, Amount}} ->
                    {ok, Amount};
                false ->
                    {error, resource_not_found}
            end;
        {aborted, _Reason} ->
            {error, planet_not_found}
    end;

get_resource(_GalaxyId, _LinkId, _BadLinkType , _Resource, _State) ->
    {error, bad_link_type}.

add_resource(GalaxyId, LinkId, planet, Resource, _State) ->
    PlanetsTable = get_planets_table(GalaxyId),
    T = fun() ->
        [Planet] = mnesia:read(PlanetsTable, LinkId),
        mnesia:write(PlanetsTable, Planet#planet{resources=lists:append(
            Planet#planet.resources, [Resource])}, write)
    end,
    case mnesia:transaction(T) of
        {atomic, ok} ->
            {ok, resource_added};
        {aborted, Reason} ->
            {error, Reason}
    end;

add_resource(_GalaxyId, _LinkId, _BadLinkType , _Resource, _State) ->
    {error, bad_link_type}.

add_structure(GalaxyId, Structure, LinkId, planet, _State) ->
    PlanetsTable = get_planets_table(GalaxyId),
    T = fun() ->
        [Planet] = mnesia:read(PlanetsTable, LinkId),
        mnesia:write(PlanetsTable, Planet#planet{structures=lists:append(
            Planet#planet.structures, [Structure])}, write)
    end,
    case mnesia:transaction(T) of
        {atomic, ok} ->
            {ok, structure_added};
        {aborted, Reason} ->
            {error, Reason}
    end;

add_structure(GalaxyId, Structure, LinkId, system, _State) ->
    SystemsTable = get_systems_table(GalaxyId),
    T = fun() ->
        [System] = mnesia:read(SystemsTable, LinkId),
        mnesia:write(SystemsTable, System#system{structures=lists:append(
            System#system.structures, [Structure])}, write)
    end,
    case mnesia:transaction(T) of
        {atomic, ok} ->
            {ok, structure_added};
        {aborted, Reason} ->
            {error, Reason}
    end;

add_structure(_GalaxyId, _Structure, _LinkId, _BadLinkType, _State) ->
    {error, bad_link_type}.

get_regions_table(GalaxyId) ->
    list_to_atom(binary_to_list(GalaxyId) ++ "_regions").

get_systems_table(GalaxyId) ->
    list_to_atom(binary_to_list(GalaxyId) ++ "_systems").

get_planets_table(GalaxyId) ->
    list_to_atom(binary_to_list(GalaxyId) ++ "_planets").

read_all_records(Table) ->
    Iterator = fun(Record, Acc) -> lists:append(Acc, [Record]) end,
    T = fun() ->
        mnesia:foldl(Iterator, [], Table)
    end,
    case mnesia:transaction(T) of
        {atomic, AllRecords} ->
            {ok, AllRecords};
        {aborted, Reason} ->
            {error, Reason}
    end.


