-module(battle_srv).
-behaviour(gen_server).

-include("../galaxy_srv/include/galaxy_defs.hrl").
-include("../faction_srv/include/faction_defs.hrl").
-include("../resource_srv/include/resource_defs.hrl").
-include("battle_defs.hrl").

-define(SERVER, ?MODULE).

-ifdef(TEST).
-compile(export_all).
-endif.

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([
    create_force/5,
    get_force/2,
	destroy_force/2,
	add_resource_to_force/3,
	add_resources_to_force/3,

    add_force_class/3,
    remove_force_class/2,

    add_force_model/5,
    remove_force_model/2,

    add_weapon_type/5,
    remove_weapon_type/2
    ]).

-record(state, {implmod, implstate}).

%% ------------------------------------------------------------------
%% gen_server API Function Definitions
%% ------------------------------------------------------------------

start_link(ImplMod) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [ImplMod], []).

%% ------------------------------------------------------------------
%% Battle Server API Function Definitions
%% ------------------------------------------------------------------
create_force(GalaxyId, DisplayName, Faction, FactionGroup, SystemName) ->
    gen_server:call(?SERVER, {create_force, GalaxyId, DisplayName,
                             Faction, FactionGroup, SystemName}).

get_force(ForceId, GalaxyId) ->
    gen_server:call(?SERVER, {get_force, ForceId, GalaxyId}).

destroy_force(Id, GalaxyId) ->
    gen_server:call(?SERVER, {destroy_force, Id, GalaxyId}).

add_resource_to_force(ForceId, GalaxyId, Resource) ->
    gen_server:call(?SERVER, {add_resource_to_force, ForceId, GalaxyId,
		Resource}).

add_resources_to_force(ForceId, GalaxyId, Resources) ->
    gen_server:call(?SERVER, {add_resources_to_force, ForceId, GalaxyId,
		Resources}).

add_force_class(Name, GalaxyId, DisplayName) ->
    gen_server:call(?SERVER, {add_force_class, Name, GalaxyId,
                              DisplayName}).

remove_force_class(Name, GalaxyId) ->
    gen_server:call(?SERVER, {remove_force_class, Name, GalaxyId}).

add_force_model(Name, GalaxyId, DisplayName, Class, Weapons) ->
    gen_server:call(?SERVER, {add_force_model, Name, GalaxyId,
                              DisplayName, Class, Weapons}).

remove_force_model(Name, GalaxyId) ->
    gen_server:call(?SERVER, {remove_force_model, Name, GalaxyId}).

add_weapon_type(Name, GalaxyId, DisplayName, Modifiers,
               BaseStrength) ->
    case validate_numeric_proplist(Modifiers)  of
        false ->
            {error, capabilties_not_numeric_proplist};
        true ->
            gen_server:call(?SERVER, {add_weapon_type, Name, GalaxyId,
		        DisplayName, Modifiers, BaseStrength})
    end.           

remove_weapon_type(Name, GalaxyId) ->
    gen_server:call(?SERVER, {remove_weapon_type, Name, GalaxyId}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([ImplMod]) ->
    State = ImplMod:init(),
    {ok, #state{implmod = ImplMod, implstate = State}}.

handle_call({create_force, GalaxyId, DisplayName, Faction,
             FactionGroup, SystemName}, _From, #state{implmod = ImplMod,
             implstate = ImplState} = State) ->
    ForceId = uuid:generate(),
    case ImplMod:force_exists(ForceId, GalaxyId, ImplState) of
        true ->
            {reply, {error, {duplicate_id, ForceId}}, State};
        false  ->
            Force = #force{
                id = ForceId,
                galaxy_id = GalaxyId,
                display_name = DisplayName,
                faction = Faction,
                faction_group = FactionGroup},
            {ok, force_created} = ImplMod:create_force(Force, ImplState),
            {ok, UpdatedForce} = move_force_to_system(
                                  Force, SystemName),
            {ok, force_updated} = ImplMod:update_force(
                              UpdatedForce, ImplState),
            {reply, {ok, ForceId}, State}
    end;

handle_call({get_force, ForceId, GalaxyId}, _From,
            #state{implmod = ImplMod, implstate = ImplState} = State) ->
    case ImplMod:get_force(ForceId, GalaxyId, ImplState) of
        {ok, Force} ->
            {reply, {ok, Force}, State};
        {error, not_found} ->
            {reply, {error, not_found}, State}
    end;

handle_call({add_resource_to_force, ForceId, GalaxyId, Resource}, _From,
            #state{implmod = ImplMod, implstate = ImplState} = State) ->
    case ImplMod:get_force(ForceId, GalaxyId, ImplState) of
        {error, not_found}->
            {reply, {error, not_found}, State};
        {ok, Force} ->
            {ok, UpdatedForce} = add_force_resources(Force, [Resource]),
            {ok, UpdatedForce2} = calculate_force_capabilities(
                             UpdatedForce, ImplMod, ImplState),
            {ok, force_updated} = ImplMod:update_force(
                                    UpdatedForce2, ImplState),
            {reply, {ok, added}, State}
    end;

handle_call({add_resources_to_force, ForceId, GalaxyId, Resources},
        _From, #state{implmod = ImplMod, implstate = ImplState} = State) ->
    case ImplMod:get_force(ForceId, GalaxyId, ImplState) of
        {error, not_found}->
            {reply, {error, not_found}, State};
        {ok, Force} ->
            {ok, UpdatedForce} = add_force_resources(Force, Resources),
            {ok, UpdatedForce2} = calculate_force_capabilities(
                             UpdatedForce, ImplMod, ImplState),
            {ok, force_updated} = ImplMod:update_force(
                                    UpdatedForce2, ImplState),
            {reply, {ok, added}, State}
    end;


handle_call({destroy_force, ForceId, GalaxyId}, _From,
            #state{implmod = ImplMod, implstate = ImplState} = State) ->
    case ImplMod:force_exists(ForceId, GalaxyId, ImplState) of
        false ->
            {reply, {error, not_found}, State};
        true ->
            ImplMod:destroy_force(ForceId, GalaxyId, ImplState),
            {reply, {ok, force_destroyed}, State}
    end;

handle_call({add_force_class, Name, GalaxyId, DisplayName}, _From,
            #state{implmod = ImplMod, implstate = ImplState} = State) ->
    case ImplMod:force_class_exists(Name, GalaxyId, ImplState) of
        true ->
            {reply, {error, force_class_exists}, State};
        false  ->
            ForceClass= #force_class{
                name = Name,
                galaxy_id = GalaxyId,
                display_name = DisplayName},
            ImplMod:add_force_class(ForceClass, ImplState),
            {reply, {ok, force_class_added}, State}
    end;

handle_call({remove_force_class, Name, GalaxyId}, _From, #state{
            implmod = ImplMod, implstate = ImplState} = State) ->
    case ImplMod:force_class_exists(Name, GalaxyId, ImplState) of
        true ->
            {ok, force_class_removed} = ImplMod:remove_force_class(
                                          Name, GalaxyId, ImplState),
                {reply, {ok, force_class_removed}, State};
        false ->
                {reply, {error, force_class_not_found}, State}
    end;

handle_call({add_force_model, Name, GalaxyId, DisplayName, Class,
             Weapons}, _From, #state{implmod = ImplMod,
                                     implstate = ImplState} = State) ->
    OnlyWeaponTypes = [element(1, Weapon) || Weapon <- Weapons],
    case {ImplMod:force_model_exists(Name, GalaxyId, ImplState),
          ImplMod:force_class_exists(Class, GalaxyId, ImplState),
          verify_all_weapon_types_exists(OnlyWeaponTypes, GalaxyId, ImplMod,
                                          ImplState)} of
        {true,_,  _} ->
            {reply, {error, force_model_exists}, State};
        {_, _, {error, NonExistingWeaponTypes}} ->
            {reply, {error, {non_existing_weapon_types,
                             NonExistingWeaponTypes}}, State};
        {_, false, _} ->
            {reply, {error, non_existing_force_class}, State};
        {false, true, ok} ->
            ForceClass= #force_model{
                name = Name,
                galaxy_id = GalaxyId,
                display_name = DisplayName,
                class = Class,
                weapons = Weapons},
            ImplMod:add_force_model(ForceClass, ImplState),
            {reply, {ok, force_model_added}, State}
    end;

handle_call({remove_force_model, Name, GalaxyId}, _From, #state{
            implmod = ImplMod, implstate = ImplState} = State) ->
    case ImplMod:force_model_exists(Name, GalaxyId, ImplState) of
        true ->
            {ok, force_model_removed} = ImplMod:remove_force_model(
                                          Name, GalaxyId, ImplState),
                {reply, {ok, force_model_removed}, State};
        false ->
                {reply, {error, force_model_not_found}, State}
    end;

handle_call({add_weapon_type, Name, GalaxyId, DisplayName, Modifiers, 
             BaseStrength}, _From, #state{implmod = ImplMod, 
             implstate = ImplState} = State) ->

    case {ImplMod:weapon_type_exists(Name, GalaxyId, ImplState),
          verify_all_force_models_exists(Modifiers, GalaxyId, ImplMod,
                                          ImplState)} of
        {true, _, _} ->
            {reply, {error, weapon_type_already_exists}, State};
        {_, _, {missing_force_models, WrongForceClasses}} ->
            {reply, {error, {non_existing_force_models,
                             WrongForceClasses}}, State};
        {false,  ok} ->
            WeaponType = #weapon_type{
                name = Name,
                galaxy_id = GalaxyId,
                display_name = DisplayName,
                modifiers = Modifiers,
                base_strength = BaseStrength},
            ImplMod:add_weapon_type(WeaponType, ImplState),
            {reply, {ok, weapon_type_added}, State}
    end;

handle_call({remove_weapon_type, Name, GalaxyId}, _From, #state{
            implmod = ImplMod, implstate = ImplState} = State) ->
    case ImplMod:weapon_type_exists(Name, GalaxyId, ImplState) of
        true ->
            {ok, weapon_type_removed} = ImplMod:remove_weapon_type(
                                          Name, GalaxyId, ImplState),
                {reply, {ok, weapon_type_removed}, State};
        false ->
                {reply, {error, weapon_type_not_found}, State}
    end;

handle_call(Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
    {nopreply, State}.

verify_all_weapon_types_exists(List, GalaxyId, ImplMod, ImplState) ->
    {ok, AllWeaponTypes} = ImplMod:get_weapon_types(GalaxyId, ImplState),
    ResultList = [lists:member(WeaponType, AllWeaponTypes) ||
        WeaponType <- List],
    case proplists:lookup_all(error, ResultList) of
        [] ->
            ok;
        ErrorList ->
            OnlyNonExistingWeaponTypes = [element(1, F) || F <- ErrorList],
            {missing_weapon_types, OnlyNonExistingWeaponTypes}
    end.

validate_numeric_proplist([]) ->
    true;

validate_numeric_proplist([{Key, Value} | List]) when 
        is_binary(Key), is_number(Value) ->
    validate_numeric_proplist(List);

validate_numeric_proplist(_NoMatch) ->
    false.

verify_all_force_models_exists(List, GalaxyId, ImplMod, ImplState) ->
    {ok, AllForceClasses} = ImplMod:get_force_models(GalaxyId, ImplState),
    OnlyAllForceClassNames = [element(1, ForceClass) ||
        ForceClass <- AllForceClasses],
    ResultList = [lists:member(ForceClass, OnlyAllForceClassNames) ||
        ForceClass <- List],
    case proplists:lookup_all(error, ResultList) of
        [] ->
            ok;
        ErrorList ->
            OnlyNonExistingForceClasses = [element(1, F) || 
                                           F <- ErrorList],
            {missing_force_models, OnlyNonExistingForceClasses}
    end.

calculate_force_capabilities(#force{resources = Resources,
                                   galaxy_id = GalaxyId} = Force,
                            ImplMod, ImplState) ->
    CapabilityDict = dict:new(),
    AllMilitaryResources = filter_force_model_resources(Resources,
                                                       GalaxyId),

    {ok, UpdatedCapabilityDict} = process_resource_capabilities(
                              AllMilitaryResources,
                              CapabilityDict,
                              ImplMod,
                              ImplState),
    {ok, Force#force{capabilities = dict:to_list(
                                      UpdatedCapabilityDict)}}.
    
process_resource_capabilities([], CapabilitiesDict, _ImplMod,
                              _ImplState) ->
    {ok, CapabilitiesDict};

process_resource_capabilities([MilitaryResource | Resouces],
                              CapabilitiesDict, ImplMod, ImplState) ->
    ResourceName = MilitaryResource#resource.name,
    GalaxyId = MilitaryResource#resource.galaxy_id,
    ResourceAmount = MilitaryResource#resource.amount,
    {ok, UpdatedCapabilitiesDict} = process_weapons_capabilities(ResourceName,
                                                     GalaxyId,
                                                     ResourceAmount,
                                                     CapabilitiesDict,
                                                     ImplMod,
                                                     ImplState),
    process_resource_capabilities(Resouces, UpdatedCapabilitiesDict,
                                   ImplMod, ImplState).

process_weapons_capabilities(ResourceName, GalaxyId, ResourceAmount,
                             Dict, ImplMod, ImplState) ->
    {ok, ResourceType} = resource_srv:get_resource_type(
                           ResourceName, GalaxyId),
    ForceModelNameRecord = ResourceType#resource_type.type,
    ForceModelName = ForceModelNameRecord#force_model_name.name,
    {ok, ForceModel} = ImplMod:get_force_model(ForceModelName, GalaxyId, 
                                         ImplState),
    %% Weapons is proplist containg the weapon type and the number of
    %% weapons, example: {<<"laser_turret">>, 6}. 
    Weapons = ForceModel#force_model.weapons,
    {ok, UpdatedDict} = update_weapons_capabilities(
                          Weapons, Dict, ResourceAmount, GalaxyId,
                          ImplMod, ImplState),
    {ok, UpdatedDict}.

update_weapons_capabilities([], Dict, _ResourceAmount, _GalaxyId,
                            _ImplMod, _ImplState) ->
    {ok, Dict};

update_weapons_capabilities([Weapon| Weapons], Dict, ResourceAmount,
                            GalaxyId, ImplMod, ImplState) ->
    {WeaponName, NumWeapons} = Weapon,
    {ok, WeaponType} = ImplMod:get_weapon_type(WeaponName, GalaxyId,
                                         ImplState),
    Modifiers  = WeaponType#weapon_type.modifiers,
    {ok, UpdatedDict} = add_capabilitites_to_dict(Modifiers,
                                                  ResourceAmount,
                                                  NumWeapons, Dict),
    update_weapons_capabilities(Weapons, UpdatedDict, ResourceAmount,
                                GalaxyId, ImplMod, ImplState).

add_capabilitites_to_dict([], _NumWeapons, _ResouceAmount, Dict) ->
    {ok, Dict};

add_capabilitites_to_dict([{Modifier, Strength} | Modifiers],
                          NumWeapons, ResourceAmount, Dict) ->
    UpdatedDict = dict:update_counter(
                    Modifier,
                    Strength * NumWeapons * ResourceAmount,
                    Dict),
    add_capabilitites_to_dict(Modifiers, NumWeapons, ResourceAmount,
                              UpdatedDict).

filter_force_model_resources(Resources, GalaxyId) ->
    ResourceNames = [Resource#resource.name || Resource <- Resources],
    {ok, ResourceTypes} = resource_srv:get_resource_types(
                              ResourceNames, GalaxyId),
    ZippedList = lists:zip(Resources, ResourceTypes),
    [Resource || {Resource, ResourceType} <- ZippedList,
        element(1, ResourceType#resource_type.type) == force_model_name].

add_force_resources(Force, []) ->
    {ok, Force};

add_force_resources(Force, [Resource | Resources]) ->
    {ok, UpdatedResourceList} = resource_helper:add_to_resource_list(
                            Resource, Force#force.resources),
    add_force_resources(Force#force{resources = UpdatedResourceList},
                        Resources).

move_force_to_system(Force, SystemName) ->
    ForceId = Force#force.id,
    GalaxyId = Force#force.galaxy_id,
    case galaxy_srv:get_system(GalaxyId, SystemName) of
        {ok, System} ->
            ForceList = System#system.forces,
            case lists:member(ForceId, ForceList) of
                true ->
                    pass;
                false ->
                    case Force#force.location of
                        undefined ->
                            pass;
                        ExistingLocation ->
                            galaxy_srv:remove_force_from_system(
                                GalaxyId, ForceId, SystemName)
                    end,
                    {ok, force_added} = galaxy_srv:add_force_to_system(
                                      GalaxyId, ForceId, SystemName),
                    UpdatedForce = Force#force{location=SystemName},
                    {ok, UpdatedForce}
            end;
        {error, not_found} ->
            {error, system_not_found}
    end.
