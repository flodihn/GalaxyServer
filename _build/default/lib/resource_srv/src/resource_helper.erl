-module(resource_helper).

-include("resource_defs.hrl").

-ifdef(TEST).
-compile(export_all).
-endif.

-export([
    add_to_resource_list/2,
    remove_from_resource_list/2
    ]).

add_to_resource_list(Resource, ResourceList) ->
    case lists:keytake(Resource#resource.name, 2, ResourceList) of
        {value, ExistingResource, NewResourceList} ->
            CurrentAmount = ExistingResource#resource.amount,
            AmountToAdd = Resource#resource.amount,
            NewAmount = CurrentAmount + AmountToAdd,
            UpdatedResource = Resource#resource{amount = NewAmount},
            UpdatedResourceList = lists:append([UpdatedResource],
                                               NewResourceList), 
            {ok, UpdatedResourceList};
        false ->
            UpdatedResourceList = lists:append(ResourceList,
                                               [Resource]),
            {ok, UpdatedResourceList}
    end.      

remove_from_resource_list(Resource, ResourceList) ->
    case lists:keytake(Resource#resource.name, 2, ResourceList) of
        {value, ExistingResource, NewResourceList} ->
            CurrentAmount = ExistingResource#resource.amount,
            AmountToRemove = Resource#resource.amount,
            NewAmount = CurrentAmount - AmountToRemove,
            case NewAmount =< 0 of
                true ->
                    {ok, NewResourceList};
                false ->
                    UpdatedResource = Resource#resource{
                                        amount = NewAmount},
                    UpdatedResourceList= lists:append(NewResourceList, 
                        [UpdatedResource]),
                    {ok, UpdatedResourceList}
            end;
        false ->
            {ok, ResourceList}
    end.      
