-module(galaxy_structure_util).

-include("galaxy_defs.hrl").

-ifdef(TEST).
-compile(export_all).
-endif.

-export([
    simulate_structure/2,
    t/0
    ]).

t() ->
    Structure = #structure{
        name = <<"small_shipyard">>,
        output_resources = [],
        input_resources = [#resource{name = <<"metal">>, amount = 25},
            #resource{name = <<"plastic">>, amount = 50}],
        output_storage_space = 970,
        input_storage_space = 0},
    simulate_structure(Structure, 1.0).

simulate_structure(Structure, DeltaTime) ->
     {ok, StructureType} = galaxy_srv:get_structure_type(
        Structure#structure.name),
    ProductionRate = StructureType#structure_type.rate,
    UpdatedStructure = produce_resources(Structure, StructureType,
        DeltaTime, ProductionRate),
    {ok, UpdatedStructure}.

produce_resources(Structure, _StructureType, _DeltaTime, 0) ->
    Structure;

produce_resources(Structure, StructureType, DeltaTime, ProductionRate) ->
    ResourceList = StructureType#structure_type.produces,
    UpdatedStructure = convert_resources(ResourceList, Structure,
        StructureType, DeltaTime),
    produce_resources(UpdatedStructure, StructureType, DeltaTime,
        ProductionRate - 1).

convert_resources([], Structure, _StructureType, _DeltaTime) ->
    Structure;

convert_resources([Resource | Rest], Structure, StructureType, DeltaTime) ->
    {ok, ResourceType} = galaxy_srv:get_resource_type(
        Resource#resource.name),

    HasBuildMaterials = has_build_materials(ResourceType, Structure),
    HasOutputStorageSpace = has_output_storage_space(Resource,
        Structure, StructureType),

    BuildMaterials = ResourceType#resource_type.build_materials,

    case {HasBuildMaterials, HasOutputStorageSpace} of
        {true, true} ->
            {ok, UpdatedStructure} = remove_input_resources(BuildMaterials,
                Structure),
            {ok, UpdatedStructure2} = add_output_resource(Resource,
                UpdatedStructure),
            convert_resources(Rest, UpdatedStructure2, StructureType,
                DeltaTime);
        {false, true} ->
            error_logger:info_report({convert_resources,
                missing_build_materials, ResourceType}),
            convert_resources(Rest, Structure, StructureType, DeltaTime);
        {true, false} ->
            error_logger:info_report({convert_resources,
                missing_storage_space}),
            convert_resources(Rest, Structure, StructureType, DeltaTime);
        {false, false} ->
            error_logger:info_report({convert_resources,
                missing_build_materials, ResourceType}),
            error_logger:info_report({convert_resources,
                missing_storage_space}),
            convert_resources(Rest, Structure, StructureType, DeltaTime)
    end.

    %UpdatedResourceList = add_resource_to_list(
    %     HourlyResource,
    %     Structure#structure.output_resources),
    % UpdatedOutputStorageSpace = OutputStorageSpace + HourlyStorageSpace,
    % UpdatedStructure = Structure#structure{
    %     output_resources = UpdatedResourceList,
    %     output_storage_space = UpdatedOutputStorageSpace},
    % convert_resources(Rest, UpdatedStructure, DeltaTime).

    %BuildingMaterials = ResourceType#resource_type.build_materials,

has_build_materials(#resource_type{} = ResourceType, Structure) ->
    BuildMaterials = ResourceType#resource_type.build_materials,
    has_build_materials(BuildMaterials, Structure);

has_build_materials([], _Structure) ->
    true;

has_build_materials([BuildMaterial | Rest], Structure) ->
    case has_build_material(BuildMaterial, Structure) of
        true ->
            has_build_materials(Rest, Structure);
        false ->
            false
    end.      
    
has_build_material(BuildMaterial, Structure) ->
    ExistingResources = Structure#structure.input_resources,
    Name = BuildMaterial#resource.name,
    RequiredAmount = BuildMaterial#resource.amount,
    case lists:keytake(Name, 2, ExistingResources) of
        {value, ExistingResource, _ResourceList} ->
            ExistingResource#resource.amount >= RequiredAmount;
        false ->
            false
    end.

has_output_storage_space(Resource, Structure, StructureType) ->
    ResourceSpace = galaxy_util:resource_storage_space(Resource),
    UsedStorageSpace = Structure#structure.output_storage_space,
    MaxStorage = StructureType#structure_type.output_storage_space,
    UsedStorageSpace + ResourceSpace =< MaxStorage.

add_output_resource(Resource, Structure) ->
    ResourceStorageSpace = galaxy_util:resource_storage_space(Resource),
    CurrentOutputStorageSpace = Structure#structure.output_storage_space,
    NewOutputStorageSpace = CurrentOutputStorageSpace +
        ResourceStorageSpace,
    ResourceList = Structure#structure.output_resources,
    case lists:keytake(Resource#resource.name, 2, ResourceList) of
        {value, ExistingResource, NewResourceList} ->
            CurrentAmount = ExistingResource#resource.amount,
            AmountToRemove = Resource#resource.amount,
            NewAmount = CurrentAmount + AmountToRemove,
            UpdatedResource = Resource#resource{amount = NewAmount},
            UpdatedStructure = Structure#structure{
                output_storage_space = NewOutputStorageSpace,
                output_resources = lists:append(NewResourceList, 
                    [UpdatedResource])},
            {ok, UpdatedStructure};
        false ->
            UpdatedStructure = Structure#structure{
                output_storage_space = NewOutputStorageSpace,
                output_resources = lists:append(ResourceList, [Resource])},
            {ok, UpdatedStructure}
    end.      

remove_input_resources([], Structure) ->
    {ok, Structure};

remove_input_resources([Resource | Rest], Structure) ->
    ResourceList = Structure#structure.input_resources,
    case lists:keytake(Resource#resource.name, 2, ResourceList) of
        {value, ExistingResource, NewResourceList} ->
            CurrentAmount = ExistingResource#resource.amount,
            AmountToRemove = Resource#resource.amount,
            NewAmount = CurrentAmount - AmountToRemove,
            NewResource = Resource#resource{amount = NewAmount},
            case NewAmount =< 0 of
                true ->
                    remove_input_resources(Rest, Structure#structure{
                        input_resources = NewResourceList});
                false ->
                    remove_input_resources(Rest, Structure#structure{
                        input_resources = lists:append(NewResourceList,
                            [NewResource])})
            end
    end.      

cap_output_capacity(Structure, StructureType, Resource, ResourceType) ->
    OutputStorageSpace = Structure#structure.output_storage_space,
    MaxOutputStorageSpace =
        StructureType#structure_type.output_storage_space,
    Amount = Resource#resource.amount,
    ResourceTypeStorageSpace = ResourceType#resource_type.storage_space,
    ResourceSpace = Amount * ResourceTypeStorageSpace, 
    case OutputStorageSpace + ResourceSpace > MaxOutputStorageSpace of
        false ->
            Resource#resource{amount = Amount};
        true ->
            MaxStorage = MaxOutputStorageSpace - OutputStorageSpace,
            MaxAmount = MaxStorage / ResourceTypeStorageSpace,
            case MaxStorage < 0 of
                true -> Resource#resource{amount = 0};
                false -> Resource#resource{amount = MaxAmount}
            end
    end.

%output_resources([], AccumulatedResources,  UpdatedStructure,
%        _StructureType, _DeltaTime) ->
%    {ok, UpdatedStructure};

%output_resources([Resource | ResourceList], AccumulatedResources, 
%        #structure{output_resources=OutputResources} = Structure,
%        StructureType,
%        DeltaTime) ->
%    Amount = Resource#resource.amount,
%    ResourceAmount = hourly_resource_rate(Amount, DeltaTime),
%    StorageSpace = Structure#structure.output_storage_space,
%    NewStorageSpace = StorageSpace + ResourceAmount,
%    case lists:keyfind(Resource#resource.name, 2, OutputResources) of
%        #resource{name=ResourceName, amount=ExistingAmount} ->
%            NewAmount = ExistingAmount + ResourceAmount,
%            NewResource = #resource{name=ResourceName, amount=NewAmount},
%            output_resources(ResourceList,
%                lists:append(AccumulatedResources, [NewResource]),
%                Structure#structure{
%                    output_storage_space=NewStorageSpace},
%                StructureType,
%                DeltaTime);
%        false ->
%            output_resources(ResourceList,
%                lists:append(AccumulatedResources, [Resource]),
%                Structure#structure{
%                    output_storage_space=NewStorageSpace},
%                StructureType,
%                DeltaTime)
%    end.

