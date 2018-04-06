-module(mnesia_resource).

-define(DB_RESOURCE_TYPE_TABLE, resource_types).
-define(DB_STRUCTURE_TYPE_TABLE, structure_types).

-include("resource_defs.hrl").

-export([
    init/0
    ]).

-export([
    create_resource_type/2,
    create_structure_type/2,
    get_resource_type/2,
    get_structure_type/2
    ]).

init() ->
    mnesia:start(),
    
    ResourceTypeAttributes = record_info(fields, resource_type),
    create_table(?DB_RESOURCE_TYPE_TABLE, resource_type,
        ResourceTypeAttributes, [category], set),

    StructureTypeAttributes = record_info(fields, structure_type),
    create_table(?DB_STRUCTURE_TYPE_TABLE, structure_type,
        StructureTypeAttributes, [category], set),
 
    {ok, []}.

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
