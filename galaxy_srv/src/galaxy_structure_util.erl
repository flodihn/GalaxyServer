-module(galaxy_structure_util).

-include("galaxy_defs.hrl").

-ifdef(TEST).
-compile(export_all).
-endif.

-export([
    simulate_structures/2,
    simulate_structure/2,
    pretty_print/1,
    t/0
    ]).

t() ->
    %Structure = #structure{
    %    name = <<"small_shipyard">>,
    %    output_resources = [],
    %    input_resources = [#resource{name = <<"metal">>, amount = 25},
    %        #resource{name = <<"plastic">>, amount = 50}],
    %    output_storage_space = 970,
    %    input_storage_space = 0},
    Structure = #structure{
        name = <<"quadanium_mine">>,
        output_resources = [],
        input_resources = [],
        output_storage_space = 970,
        input_storage_space = 0},
 
    simulate_structure(Structure, 3.0).

simulate_structures(StructureList, DeltaTime) ->
    simulate_structures(StructureList, [], DeltaTime).

simulate_structures([], UpdatedStructures, _DeltaTime) ->
    {ok, UpdatedStructures};

simulate_structures([Structure | Rest], UpdatedStructures, DeltaTime) ->
    {ok, UpdatedStructure} = simulate_structure(Structure, DeltaTime),
    simulate_structures(Rest, lists:append(
        UpdatedStructures, [UpdatedStructure]), DeltaTime).

simulate_structure(Structure, DeltaTime) ->
     {ok, StructureType} = galaxy_srv:get_structure_type(
        Structure#structure.name),
    ResourceList = StructureType#structure_type.produces,
    {ok, UpdatedStructure} = create_or_convert_resources(ResourceList,
        Structure, StructureType, DeltaTime),
    {ok, UpdatedStructure2} = process_build_queue(UpdatedStructure),
    {ok, UpdatedStructure2}.

process_build_queue(Structure) ->
    BuildQueue = Structure#structure.build_queue,
    process_build_queue(BuildQueue, [], Structure).

process_build_queue([], NewBuildQueue, Structure) ->
    {ok, Structure#structure{build_queue = NewBuildQueue}};

process_build_queue([QueueItem | Rest], NewBuildQueue, Structure) ->
    FinishTime = QueueItem#queue_item.finish_time,
    Resource = QueueItem#queue_item.resource,
    case has_timestamp_happened(FinishTime) of
        true ->
            {ok, UpdatedStructure} = add_output_resource(Resource,
                Structure),
            %gen_event:notify(?GALAXY_RESOURCE_EVENT_MANAGER,
            %    {resource_created, Resource}),
            process_build_queue(Rest, NewBuildQueue, UpdatedStructure);
        false ->
            UpdatedBuildQueue = lists:append(NewBuildQueue, [QueueItem]),
            process_build_queue(Rest, UpdatedBuildQueue, Structure)
    end.

has_timestamp_happened(TimeStamp) ->
    Now = erlang:timestamp(),
    timer:now_diff(Now, TimeStamp) >= 0.

create_or_convert_resources([], Structure, _StructureType, _DeltaTime) ->
    {ok, Structure};

create_or_convert_resources([Resource | Rest], Structure, StructureType,
        DeltaTime) ->
    {ok, ResourceType} = galaxy_srv:get_resource_type(
        Resource#resource.name),
    case ResourceType#resource_type.build_materials of
        [] ->
            {ok, UpdatedStructure} = create_resource(Resource, ResourceType,
                Structure, StructureType, DeltaTime),
            create_or_convert_resources(Rest, UpdatedStructure,
                StructureType, DeltaTime);
        _BuildMaterials ->
            {ok, UpdatedStructure} = convert_resource(Resource,
                ResourceType, Structure, StructureType),
            create_or_convert_resources(Rest, UpdatedStructure,
                StructureType, DeltaTime)
    end.

create_resource(Resource, ResourceType, Structure, StructureType,
        DeltaTime) ->
    Amount = Resource#resource.amount,
    HourlyResourceAmount = galaxy_util:hourly_resource_rate(Amount,
        DeltaTime),
    HourlyResource = Resource#resource{amount = HourlyResourceAmount},    

    HasOutputStorageSpace = has_output_storage_space(HourlyResource,
        Structure, StructureType),

    case HasOutputStorageSpace of
        true ->
            {ok, UpdatedStructure} = add_output_resource(HourlyResource,
                Structure),
            %gen_event:notify(?GALAXY_RESOURCE_EVENT_MANAGER,
            %    {resource_created, HourlyResource}),
            {ok, UpdatedStructure};
        false ->
            CappedResource = cap_output_capacity(Structure, StructureType,
                HourlyResource, ResourceType),
            {ok, UpdatedStructure} = add_output_resource(CappedResource,
                Structure),
            {ok, UpdatedStructure}
    end.

convert_resource(Resource, ResourceType, Structure, StructureType) ->
    HasBuildMaterials = has_build_materials(ResourceType, Structure),
    HasOutputStorageSpace = has_output_storage_space(Resource,
            Structure, StructureType),

    case {HasBuildMaterials, HasOutputStorageSpace} of
        {true, true} ->
            {ok, UpdatedStructure} = add_build_queue(Resource, ResourceType,
                    Structure, StructureType),
            {ok, UpdatedStructure};
        {_, _} ->
            {ok, Structure}
    end.

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

add_build_queue(Resource, ResourceType, Structure, StructureType) ->
    case reached_max_production_rate(Structure, StructureType) of
        true ->
            {ok, Structure};
        false ->
            BuildQueue = Structure#structure.build_queue,
            BuildTime = ResourceType#resource_type.build_time,
            FinishTime = get_finish_time(BuildTime),
            QueueItem = #queue_item{
                resource = Resource,
                finish_time = FinishTime},
            UpdatedBuildQueue = lists:append(BuildQueue, [QueueItem]),
            BuildMaterials = ResourceType#resource_type.build_materials,
            {ok, UpdatedStructure} = remove_input_resources(BuildMaterials,
                Structure),
            {ok, UpdatedStructure#structure{
                build_queue = UpdatedBuildQueue}}
    end.    

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

            %gen_event:notify(?GALAXY_RESOURCE_EVENT_MANAGER,
            %    {resource_removed, Resource}),

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

get_finish_time(BuildTime) ->
    {MegaSeconds, Seconds, _MilliSeconds} = erlang:timestamp(),
    FinishTime = {MegaSeconds, Seconds + BuildTime, 0},
    FinishTime.

reached_max_production_rate(Structure, StructureType) ->
    ProductionRate = StructureType#structure_type.production_rate,
    BuildQueue = Structure#structure.build_queue,
    length(BuildQueue) >= ProductionRate.

pretty_print(#structure{
        uid = Uid,
        name = Name,
        build_queue = BuildQueue,
        output_resources = OutputResources,
        input_resources = InputResources,
        output_storage_space = OutputStorageSpace,
        input_storage_space = InputStorageSpace}) ->
    io:format("~n========== STRUCTURE ==========~n", []),
    io:format("uid: ~p~n", [Uid]),
    io:format("name: ~p~n", [Name]),
    io:format("build_queue: ~p~n", [BuildQueue]),
    io:format("output_resources: ~p~n", [OutputResources]),
    io:format("input_resources: ~p~n", [InputResources]),
    io:format("output_storage_space: ~p~n", [OutputStorageSpace]),
    io:format("input_storage_space: ~p~n", [InputStorageSpace]),
    ok.

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

