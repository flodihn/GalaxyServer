-module(mnesia_battle).

-include("battle_defs.hrl").

-include_lib("stdlib/include/qlc.hrl").

-ifdef(TEST).
-compile(export_all).
-endif.

-export([
    init/0
    ]).

-export([
    create_force/2,
    force_exists/3,
    destroy_force/3,

    add_force_model/2,
	remove_force_model/3,
    get_force_model/3,
	force_model_exists/3,
	get_force_models/2,

    add_force_class/2,
	remove_force_class/3,
    get_force_class/3,
	force_class_exists/3,

	add_weapon_type/2,
	remove_weapon_type/3,
    weapon_type_exists/3,
    get_weapon_type/3,
	get_weapon_types/2,
	get_weapon_types/3
    ]).

init() ->
    mnesia:start(),
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

find_or_create_force_table(TableName) ->
	case lists:member(TableName, mnesia:system_info(tables)) of
		true ->
			pass;
		false ->
    		TypeAttributes = record_info(fields, force),
    		create_table(TableName, force,
                         TypeAttributes, [display_name], set)
	end.


find_or_create_force_model_table(TableName) ->
	case lists:member(TableName, mnesia:system_info(tables)) of
		true ->
			pass;
		false ->
    		TypeAttributes = record_info(fields, force_model),
    		create_table(TableName, force_model,
        		TypeAttributes, [], set)
	end.

find_or_create_force_class_table(TableName) ->
	case lists:member(TableName, mnesia:system_info(tables)) of
		true ->
			pass;
		false ->
    		TypeAttributes = record_info(fields, force_class),
    		create_table(TableName, force_class,
        		TypeAttributes, [], set)
	end.

find_or_create_weapon_types_table(TableName) ->
	case lists:member(TableName, mnesia:system_info(tables)) of
		true ->
			pass;
		false ->
    		ResourceTypeAttributes = record_info(fields, weapon_type),
    		create_table(TableName, weapon_type,
        		ResourceTypeAttributes, [galaxy_id], set)
	end.

create_indexes(_TableName, []) ->
    ok;

create_indexes(TableName, [Field | Rest]) ->
    mnesia:add_table_index(TableName, Field),
    create_indexes(TableName, Rest).

change_to_disc_schema() ->
    mnesia:change_config(extra_db_nodes, [node()]),
    mnesia:change_table_copy_type(schema, node(), disc_copies).

create_force(#force{galaxy_id=GalaxyId} = Force, _State) ->
	ForceTable = get_force_table(GalaxyId),
	find_or_create_force_table(ForceTable),
    T = fun() ->
        mnesia:write(ForceTable, Force, write)
    end,
    case mnesia:transaction(T) of
        {atomic, ok} ->
            {ok, force_created};
        {aborted, Reason} ->
            {error, Reason}
    end.

force_exists(Id, GalaxyId, _State) ->
	ForceTable = get_force_table(GalaxyId),
    case table_exists(ForceTable) of
        false ->
            false;
        true ->
            T = fun() ->
                mnesia:read(ForceTable, Id)
            end,
            case mnesia:transaction(T) of
                {atomic, []} -> false;
		        {atomic, [_FoundForce]} -> true
            end
    end.

destroy_force(Id, GalaxyId, _State) ->
	ForceTable = get_force_table(GalaxyId),
	T = fun() ->
        mnesia:delete(ForceTable, Id, write)
    end,
    case mnesia:transaction(T) of
        {atomic, ok} ->
            {ok, force_destroyed};
        {aborted, Reason} ->
            {error, Reason}
    end.

add_force_model(#force_model{galaxy_id=GalaxyId} = ForceModel, _State) ->
	ForceModelTable = get_force_model_table(GalaxyId),
	find_or_create_force_model_table(ForceModelTable),
    T = fun() ->
        mnesia:write(ForceModelTable, ForceModel, write)
    end,
    case mnesia:transaction(T) of
        {atomic, ok} ->
            {ok, force_model_added};
        {aborted, Reason} ->
            {error, Reason}
    end.

get_force_model(Name, GalaxyId, _State) ->
	ForceModelTable = get_force_model_table(GalaxyId),
    case table_exists(ForceModelTable) of
        false ->
            false;
        true ->
            T = fun() ->
                mnesia:read(ForceModelTable, Name)
            end,
            case mnesia:transaction(T) of
                {atomic, []} ->
                    {error, not_found};
		        {atomic, [ForceModel]} ->
                    {ok, ForceModel} 
            end
    end.

force_model_exists(Name, GalaxyId, _State) ->
	ForceModelTable = get_force_model_table(GalaxyId),
    case table_exists(ForceModelTable) of
        false ->
            false;
        true ->
            T = fun() ->
                mnesia:read(ForceModelTable, Name)
            end,
            case mnesia:transaction(T) of
                {atomic, []} -> false;
		        {atomic, [_FoundModel]} -> true
            end
    end.

get_force_models(GalaxyId, _State) ->
	ForceModelTable = get_force_model_table(GalaxyId),
    case table_exists(ForceModelTable) of
        false -> [];
        true -> read_all_records(ForceModelTable)
    end.

remove_force_model(ForceModel, GalaxyId, _State) ->
	ForceModelTable = get_force_model_table(GalaxyId),
	T = fun() ->
        mnesia:delete(ForceModelTable, ForceModel, write)
    end,
    case mnesia:transaction(T) of
        {atomic, ok} ->
            {ok, force_model_removed};
        {aborted, Reason} ->
            {error, Reason}
    end.

add_force_class(#force_class{galaxy_id=GalaxyId} = ForceClass, _State) ->
	ForceClassTable = get_force_class_table(GalaxyId),
	find_or_create_force_class_table(ForceClassTable),
    T = fun() ->
        mnesia:write(ForceClassTable, ForceClass, write)
    end,
    case mnesia:transaction(T) of
        {atomic, ok} ->
            {ok, force_class_added};
        {aborted, Reason} ->
            {error, Reason}
    end.

get_force_class(Name, GalaxyId, _State) ->
	ForceClassTable = get_force_class_table(GalaxyId),
    case table_exists(ForceClassTable) of
        false ->
            false;
        true ->
            T = fun() ->
                mnesia:read(ForceClassTable, Name)
            end,
            case mnesia:transaction(T) of
                {atomic, []} ->
                    {error, not_found};
		        {atomic, [ForceClass]} ->
                    {ok, ForceClass} 
            end
    end.

force_class_exists(Name, GalaxyId, _State) ->
	ForceClassTable = get_force_class_table(GalaxyId),
    case table_exists(ForceClassTable) of
        false ->
            false;
        true ->
            T = fun() ->
                mnesia:read(ForceClassTable, Name)
            end,
            case mnesia:transaction(T) of
                {atomic, []} -> false;
		        {atomic, [_FoundClass]} -> true
            end
    end.

get_force_classes(GalaxyId, _State) ->
	ForceClassTable = get_force_class_table(GalaxyId),
    case table_exists(ForceClassTable) of
        false -> [];
        true -> read_all_records(ForceClassTable)
    end.

remove_force_class(ForceClass, GalaxyId, _State) ->
	ForceClassTable = get_force_class_table(GalaxyId),
	T = fun() ->
        mnesia:delete(ForceClassTable, ForceClass, write)
    end,
    case mnesia:transaction(T) of
        {atomic, ok} ->
            {ok, force_class_removed};
        {aborted, Reason} ->
            {error, Reason}
    end.

add_weapon_type(#weapon_type{galaxy_id = GalaxyId} = Weapon, _State) ->
	WeaponTypesTable = get_weapon_types_table(GalaxyId),
	find_or_create_weapon_types_table(WeaponTypesTable),
    T = fun() ->
        mnesia:write(WeaponTypesTable, Weapon, write)
    end,
    case mnesia:transaction(T) of
        {atomic, ok} ->
            {ok, weapon_type_added};
        {aborted, Reason} ->
            {error, Reason}
    end.

remove_weapon_type(Name, GalaxyId, _State) ->
	WeaponTypeTable = get_weapon_types_table(GalaxyId),
	T = fun() ->
        mnesia:delete(WeaponTypeTable, Name, write)
    end,
    case mnesia:transaction(T) of
        {atomic, ok} ->
            {ok, weapon_type_removed};
        {aborted, Reason} ->
            {error, Reason}
    end.

weapon_type_exists(Name, GalaxyId, _State) ->
	WeaponTypesTable = get_weapon_types_table(GalaxyId),
    case table_exists(WeaponTypesTable) of
        false ->
            false;
        true ->
            find_or_create_weapon_types_table(GalaxyId),
            T = fun() ->
                mnesia:read(WeaponTypesTable, Name)
            end,
            case mnesia:transaction(T) of
                {atomic, []} -> false;
		        {atomic, [_FoundWeaponType]} -> true
            end
    end.

get_weapon_type(Name, GalaxyId, _State) ->
	WeaponTypesTable = get_weapon_types_table(GalaxyId),
    case table_exists(WeaponTypesTable) of
        false ->
            false;
        true ->
            find_or_create_weapon_types_table(GalaxyId),
            T = fun() ->
                mnesia:read(WeaponTypesTable, Name)
            end,
            case mnesia:transaction(T) of
                {atomic, []} -> {error, not_found};
		        {atomic, [WeaponType]} -> {ok, WeaponType} 
            end
    end.

get_weapon_types(GalaxyId, _State) ->
	WeaponTypesTable = get_weapon_types_table(GalaxyId),
    case table_exists(WeaponTypesTable) of
        false -> [];
        true -> read_all_records(WeaponTypesTable)
    end.

get_weapon_types(Names, GalaxyId, _State) ->
	WeaponTypesTable = get_weapon_types_table(GalaxyId),
    F = fun() ->
        Q = qlc:q([W || W <- mnesia:table(WeaponTypesTable),
            lists:member(W#weapon_type.name, Names)]),
	    qlc:e(Q)
    end,
    case mnesia:transaction(F) of
        {atomic, WeaponTypes} ->
            {ok, WeaponTypes};
        {error, Reason} ->
            error_logger:error_report({?MODULE, get_weapon_types,
                                       Reason})
    end.

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

get_force_table(GalaxyId) ->
    list_to_atom(binary_to_list(GalaxyId) ++ "_forces").

get_force_model_table(GalaxyId) ->
    list_to_atom(binary_to_list(GalaxyId) ++ "_force_models").

get_force_class_table(GalaxyId) ->
    list_to_atom(binary_to_list(GalaxyId) ++ "_force_class").

get_weapon_types_table(GalaxyId) ->
    list_to_atom(binary_to_list(GalaxyId) ++ "_weapon_types").

table_exists(TableName) ->
    lists:member(TableName, mnesia:system_info(tables)).
