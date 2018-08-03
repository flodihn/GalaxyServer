-module(mnesia_faction).

-define(DB_FACTION_TABLE, factions).

-include("faction_defs.hrl").

-export([
    init/0
    ]).

-export([
	has_factions/2,
	create_faction/2,
	get_factions/2,
	get_faction/3,
    update_faction/2
    ]).

init() ->
    mnesia:start(),
    {ok, []}.

has_factions(GalaxyId, _State) ->
	FactionTable = get_faction_table(GalaxyId),
	lists:member(FactionTable, mnesia:system_info(tables)).

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
    %mnesia:add_table_index(TableName, Field),
    %create_indexes(TableName, Rest).
	ok.

change_to_disc_schema() ->
    mnesia:change_config(extra_db_nodes, [node()]),
    mnesia:change_table_copy_type(schema, node(), disc_copies).

create_faction(#faction{galaxy_id=GalaxyId} = Faction, _State) ->
	FactionTable = get_faction_table(GalaxyId),
	find_or_create_faction_table(FactionTable),
    T = fun() ->
        mnesia:write(FactionTable, Faction, write)
    end,
    case mnesia:transaction(T) of
            {atomic, ok} ->
            {ok, faction_created};
        {aborted, Reason} ->
            {error, Reason}
    end.

claim_exists(Claim, GalaxyId, _State) ->
	FactionClaimTable = get_faction_claim_table(GalaxyId),
	find_or_create_faction_claim_table(GalaxyId),
    T = fun() ->
        mnesia:read(FactionClaimTable, Claim)
    end,
    case mnesia:transaction(T) of
        {atomic, [FactionClaim]} ->
            {ok, FactionClaim};
        {aborted, []} ->
            false
    end.
    
update_faction(#faction{galaxy_id=GalaxyId} = Faction, _State) ->
	FactionTable = get_faction_table(GalaxyId),
	find_or_create_faction_table(GalaxyId),
    T = fun() ->
        mnesia:write(FactionTable, Faction, write)
    end,
    case mnesia:transaction(T) of
        {atomic, ok} ->
            {ok, faction_updated};
        {aborted, Reason} ->
            {error, Reason}
    end.

find_or_create_faction_table(TableName) ->
	case lists:member(TableName, mnesia:system_info(tables)) of
		true ->
			pass;
		false ->
    		ResourceTypeAttributes = record_info(fields, faction),
    		create_table(TableName, faction,
        		ResourceTypeAttributes, [category], set)
	end.

find_or_create_faction_claim_table(TableName) ->
	case lists:member(TableName, mnesia:system_info(tables)) of
		true ->
			pass;
		false ->
    		ResourceTypeAttributes = record_info(fields, faction_claim),
    		create_table(TableName, faction_claims,
        		ResourceTypeAttributes, [category], set)
	end.

get_factions(GalaxyId, _State) ->
	FactionTable = get_faction_table(GalaxyId),
	read_all_records(FactionTable).

get_faction(FactionName, GalaxyId, _State) ->
	FactionTable = get_faction_table(GalaxyId),
    T = fun() ->
        mnesia:read(FactionTable, FactionName)
    end,
    case mnesia:transaction(T) of
        {atomic, [Faction]} ->
            {ok, Faction};
        {aborted, _Reason} ->
            {error, faction_not_found}
    end.

get_resource_type(ResourceName, _State) ->
    %T = fun() ->
    %    mnesia:read(?DB_RESOURCE_TYPE_TABLE, ResourceName)
    %end,
    %case mnesia:transaction(T) of
    %    {atomic, [ResourceType]} ->
    %        {ok, ResourceType};
    %    {atomic, []} ->
    %        {error, not_found};
    %    {aborted, Reason} ->
    %        {error, Reason}
    %end.
	ok.

remove_resource_type(ResourceName, _State) ->
	%T = fun() ->
    %    mnesia:delete(?DB_RESOURCE_TYPE_TABLE, ResourceName, write)
    %end,
    %case mnesia:transaction(T) of
    %    {atomic, ok} ->
    %        {ok, resource_removed};
    %    {aborted, Reason} ->
    %        {error, Reason}
    %end.
	ok.

create_structure_type(StructureType, _State) ->
    %T = fun() ->
    %    mnesia:write(?DB_STRUCTURE_TYPE_TABLE, StructureType, write)
    %end,
    %case mnesia:transaction(T) of
    %    {atomic, ok} ->
    %        {ok, structure_type_created};
    %    {aborted, Reason} ->
    %        {error, Reason}
    %end.
	ok.

%get_structure_types(_State) ->
%	read_all_records(?DB_STRUCTURE_TYPE_TABLE).

get_structure_type(StructureName, _State) ->
    %T = fun() ->
    %    mnesia:read(?DB_STRUCTURE_TYPE_TABLE, StructureName)
    %end,
    %case mnesia:transaction(T) of
    %    {atomic, [StructureType]} ->
    %        {ok, StructureType};
    %    {aborted, _Reason} ->
    %        {error, planet_not_found}
    %end.
	ok.

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

get_faction_table(GalaxyId) ->
    list_to_atom(binary_to_list(GalaxyId) ++ "_factions").

get_faction_claim_table(GalaxyId) ->
    list_to_atom(binary_to_list(GalaxyId)  ++ "_faction_claims").
