-module(mnesia_resource).

-include("resource_defs.hrl").
-include("../galaxy_srv/include/galaxy_defs.hrl").

-export([
    init/0
    ]).

-export([
    destroy_resource_tables/1,
    create_resource_type/2,
    create_structure_type/2,
	get_resource_types/2,
    get_resource_type/3,
	remove_resource_type/3,
	get_structure_types/2,
    get_structure_type/3,
	remove_structure_type/2
    ]).

init() ->
    mnesia:start(),
    {ok, []}.

destroy_resource_tables(GalaxyId) ->
    ResourceTable = get_resource_type_table(GalaxyId),
    StructuresTable = get_structure_type_table(GalaxyId),
    mnesia:delete_table(ResourceTable),
    mnesia:delete_table(StructuresTable),
    ok.

find_or_create_resource_type_table(TableName) ->
	case lists:member(TableName, mnesia:system_info(tables)) of
        true ->
            pass;
        false ->
            ResourceTypeAttributes = record_info(fields, resource_type),
            create_table(TableName, resource_type,
                ResourceTypeAttributes, [galaxy_id, category], set)
    end.

find_or_create_structure_type_table(TableName) ->
	case lists:member(TableName, mnesia:system_info(tables)) of
        true ->
            pass;
        false ->
            StructureTypeAttributes = record_info(fields,
                                                  structure_type),
            create_table(TableName, structure_type,
                         StructureTypeAttributes,
                         [galaxy_id, category], set)
    end.

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

create_resource_type(
        #resource_type{galaxy_id = GalaxyId} = ResourceType, _State) ->
    Table = get_resource_type_table(GalaxyId),
    find_or_create_resource_type_table(Table),
    T = fun() ->
        mnesia:write(Table, ResourceType, write)
    end,
    case mnesia:transaction(T) of
        {atomic, ok} ->
            {ok, resource_type_created};
        {aborted, Reason} ->
            {error, Reason}
    end.

get_resource_types(GalaxyId, _State) ->
    Table = get_resource_type_table(GalaxyId),
    find_or_create_resource_type_table(Table),
    read_all_records(Table).

get_resource_type(GalaxyId, ResourceName, _State) ->
    Table = get_resource_type_table(GalaxyId),
    find_or_create_resource_type_table(Table),
    T = fun() ->
        mnesia:read(Table, ResourceName)
    end,
    case mnesia:transaction(T) of
        {atomic, [ResourceType]} ->
            {ok, ResourceType};
        {atomic, []} ->
            {error, not_found};
        {aborted, Reason} ->
            {error, Reason}
    end.

remove_resource_type(ResourceName, GalaxyId, _State) ->
    Table = get_resource_type_table(GalaxyId),
    find_or_create_resource_type_table(Table),
	T = fun() ->
        mnesia:delete(Table, ResourceName, write)
    end,
    case mnesia:transaction(T) of
        {atomic, ok} ->
            {ok, resource_removed};
        {aborted, Reason} ->
            {error, Reason}
    end.

create_structure_type(
        #structure_type{galaxy_id = GalaxyId} = StructureType,
        _State) ->
    Table = get_structure_type_table(GalaxyId),
    find_or_create_structure_type_table(Table),
    T = fun() ->
        mnesia:write(Table, StructureType, write)
    end,
    case mnesia:transaction(T) of
        {atomic, ok} ->
            {ok, structure_type_created};
        {atomic, []} ->
            {error, not_found};
        {aborted, Reason} ->
            {error, Reason}
    end.

get_structure_types(GalaxyId, _State) ->
    Table = get_structure_type_table(GalaxyId),
    find_or_create_structure_type_table(Table),
	read_all_records(Table).

get_structure_type(StructureName, GalaxyId, _State) ->
    Table = get_structure_type_table(GalaxyId),
    find_or_create_structure_type_table(Table),
    T = fun() ->
        mnesia:read(Table, StructureName)
    end,
    case mnesia:transaction(T) of
        {atomic, [StructureType]} ->
            {ok, StructureType};
        {atomic, []} ->
            {error, not_found};
        {aborted, _Reason} ->
            {error, planet_not_found}
    end.

remove_structure_type(
        #structure_type{galaxy_id = GalaxyId} = StructureType,
        _State) ->
    Table = get_structure_type_table(GalaxyId),
    find_or_create_structure_type_table(Table),
	T = fun() ->
        mnesia:delete(Table, StructureType, write)
    end,
    case mnesia:transaction(T) of
        {atomic, ok} ->
            {ok, structure_type_removed};
        {aborted, Reason} ->
            {error, Reason}
    end.

get_resource_type_table(GalaxyId) ->
    list_to_atom(binary_to_list(GalaxyId) ++ "_resource_types").

get_structure_type_table(GalaxyId) ->
    list_to_atom(binary_to_list(GalaxyId) ++ "_structure_types").

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
