-module(galaxy_structure_util).

-include("galaxy_defs.hrl").

-ifdef(TEST).
-compile(export_all).
-endif.

%% Application callbacks
-export([
    simulate_structure/2
    ]).

simulate_structure(Structure, DeltaTime) ->
     {ok, StructureType} = galaxy_srv:get_structure_type(
        Structure#structure.name),
    ResourceList = StructureType#structure_type.produces,
    UpdatedStructure = convert_resources(ResourceList, Structure,
        DeltaTime),
    {ok, UpdatedStructure}.

convert_resources([], Structure, _DeltaTime) ->
    Structure;

convert_resources([Resource | Rest], Structure, DeltaTime) ->
    {ok, ResourceType} = galaxy_srv:get_resource_type(
        Resource#resource.name),
    OutputStorageSpace = Structure#structure.output_storage_space,
    ResourceTypeStorageSpace = ResourceType#resource_type.storage_space,
    HourlyResourceRate = hourly_resource_rate(
        Resource#resource.amount, DeltaTime),
    HourlyStorageSpace = HourlyResourceRate * ResourceTypeStorageSpace,
    ResourceSpace = HourlyResourceRate * ResourceTypeStorageSpace,
    HourlyResource = Resource#resource{amount = HourlyResourceRate},
    UpdatedResourceList = add_resource_to_list(
        HourlyResource,
        Structure#structure.output_resources),
    UpdatedOutputStorageSpace = OutputStorageSpace + HourlyStorageSpace,
    UpdatedStructure = Structure#structure{
        output_resources = UpdatedResourceList,
        output_storage_space = UpdatedOutputStorageSpace},
    convert_resources(Rest, UpdatedStructure, DeltaTime).

add_resource_to_list(NewResource, ExistingResources) ->
     case lists:keytake(NewResource#resource.name, 2, ExistingResources) of
        {value, ExistingResource, ResourceList} ->
            #resource{
                name=ExistingName,
                amount=ExistingAmount} = ExistingResource,
            NewAmount = ExistingAmount + NewResource#resource.amount,
            lists:append(ResourceList, [
                #resource{name=ExistingName, amount=NewAmount}]);
        false ->
            lists:append(ExistingResources, [NewResource])
    end.

cap_output_capacity(Structure, StructureType, Resource) ->
    OutputStorageSpace = Structure#structure.output_storage_space,
    MaxOutputStorageSpace =
        StructureType#structure_type.output_storage_space,
    Amount = Resource#resource.amount,
    case OutputStorageSpace + Amount > MaxOutputStorageSpace of
        false ->
            Resource#resource{amount = Amount};
        true ->
            MaxStorage = MaxOutputStorageSpace - OutputStorageSpace,
            case MaxStorage < 0 of
                true -> Resource#resource{amount = 0};
                false -> Resource#resource{amount = MaxStorage}
            end
    end.

output_resources([], AccumulatedResources,  UpdatedStructure,
        _StructureType, _DeltaTime) ->
    {ok, UpdatedStructure};

output_resources([Resource | ResourceList], AccumulatedResources, 
        #structure{output_resources=OutputResources} = Structure,
        StructureType,
        DeltaTime) ->
    Amount = Resource#resource.amount,
    ResourceAmount = hourly_resource_rate(Amount, DeltaTime),
    StorageSpace = Structure#structure.output_storage_space,
    NewStorageSpace = StorageSpace + ResourceAmount,
    case lists:keyfind(Resource#resource.name, 2, OutputResources) of
        #resource{name=ResourceName, amount=ExistingAmount} ->
            NewAmount = ExistingAmount + ResourceAmount,
            NewResource = #resource{name=ResourceName, amount=NewAmount},
            output_resources(ResourceList,
                lists:append(AccumulatedResources, [NewResource]),
                Structure#structure{
                    output_storage_space=NewStorageSpace},
                StructureType,
                DeltaTime);
        false ->
            output_resources(ResourceList,
                lists:append(AccumulatedResources, [Resource]),
                Structure#structure{
                    output_storage_space=NewStorageSpace},
                StructureType,
                DeltaTime)
    end.

hourly_resource_rate(Amount, DeltaTime) ->
    truncate(1/3600 * Amount * DeltaTime, 6).

truncate(F, N) ->
    Prec = math:pow(10, N),
    trunc(F * Prec) / Prec.
