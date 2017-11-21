-module(mnesia_galaxy).

-define(DB_GALAXY_TABLE, galaxy).

-include("galaxy_defs.hrl").

-export([
    init/0
    ]).

-export([
    create_galaxy/2,
    create_region/3,
    system_exists/3,
    create_system/4,
    list_systems/2
    ]).

init() ->
    mnesia:start(),
    GalaxyAttributes = record_info(fields, galaxy),
    create_table(?DB_GALAXY_TABLE, GalaxyAttributes, []),
   {ok, []}.

create_galaxy_tables(GalaxyId) ->
    RegionsTable = get_regions_table(GalaxyId),
    RegionAttributes = record_info(fields, region),
    create_table(RegionsTable, RegionAttributes, []),

    SystemsTable = get_systems_table(GalaxyId),
    SystemAttributes = record_info(fields, system),
    create_table(SystemsTable, SystemAttributes, [pos]).
 
create_table(TableName, Attributes, IndexList) ->
    case lists:member(TableName, mnesia:system_info(tables)) of
        true ->
            {ok, already_exists};
        false ->
            change_to_disc_schema(),
            mnesia:create_table(TableName, 
                [{disc_copies, [node()]},
                {type, set},
                {attributes, Attributes}]),
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

create_region(GalaxyId, Region = #region{}, State) ->
    RegionTable = get_regions_table(GalaxyId),
    WriteRegion = fun() ->
        mnesia:write(RegionTable, Region, write)
    end,
    case mnesia:transaction(WriteRegion) of
        {atomic, ok} ->
            {ok, region_created};
        {aborted, Reason} ->
            {error, Reason}
    end;

create_region(GalaxyId, Region, _State) when is_atom(GalaxyId) ->
    {error, not_region_record};

create_region(RegionId, Region = #region{}, State) ->
    {error, region_id_not_atom};

create_region(_, _, _) ->
    {error, unknown}.

create_system(GalaxyId, RegionId, System = #system{}, _State) ->
    SystemsTable = get_systems_table(GalaxyId),
    RegionsTable = get_regions_table(GalaxyId),
    WriteSystem = fun() ->
        {ok, Region} = mnesia:read(RegionsTable, RegionId),
        mnesia:write(SystemsTable, System, write),
        mnesia:write(RegionsTable, Region#region{systems=lists:append(
            Region#region.systems, System#system.id)}, write)
    end,
    case mnesia:transaction(WriteSystem) of
        {atomic, ok} ->
            {ok, system_created};
        {aborted, Reason} ->
            {error, Reason}
    end;

create_system(_GalaxyId, RegionId, System, _State) when is_atom(RegionId) ->
    {error, not_system_record};

create_system(_GalaxyId, RegionId, System = #system{}, _State) ->
    {error, region_id_not_atom};

create_system(_, _, _, _) ->
    {error, unknown}.

list_systems(GalaxyId, _State) ->
    SystemsTable = get_systems_table(GalaxyId),
    [X -> 

get_regions_table(GalaxyId) ->
    list_to_atom(binary_to_list(GalaxyId) ++ "_regions").

get_systems_table(GalaxyId) ->
    list_to_atom(binary_to_list(GalaxyId) ++ "_systems").
