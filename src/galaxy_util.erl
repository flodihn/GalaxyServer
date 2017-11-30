-module(galaxy_util).

-include("galaxy_defs.hrl").

%% Application callbacks
-export([
    sector_name_to_sup/1,
    hourly_resource_rate/2,
    resource_storage_space/1
    ]).

sector_name_to_sup(SectorName) ->
    list_to_atom(atom_to_list(SectorName) ++ "_sup").

hourly_resource_rate(Amount, DeltaTime) ->
    truncate(1/3600 * Amount * DeltaTime, 6).

resource_storage_space(#resource{name = Name, amount = Amount}) ->
    {ok, ResourceType} = galaxy_srv:get_resource_type(Name),
    ResourceType#resource_type.storage_space * Amount;

resource_storage_space(ResourceList) when is_list(ResourceList) ->
    resource_storage_space(ResourceList, 0).

resource_storage_space([], Acc) ->
    Acc;

%add_output_resource(NewResource, Structure) ->
%    ExistingResource = Structure#structure.output_resources
%     case lists:keytake(NewResource#resource.name, 2, ExistingResources) of
%        {value, ExistingResource, ResourceList} ->
%            #resource{
%                name=ExistingName,
%                amount=ExistingAmount} = ExistingResource,
%            NewAmount = ExistingAmount + NewResource#resource.amount,
%            lists:append(ResourceList, [
%                #resource{name=ExistingName, amount=NewAmount}]);
%        false ->
%            lists:append(ExistingResources, [NewResource])
%    end.

resource_storage_space([Resource | Rest], Acc) ->
    StorageSpace = galaxy_srv:get_resource_type(Resource),
    resource_storage_space(Rest, Acc + StorageSpace).

truncate(F, N) ->
    Prec = math:pow(10, N),
    trunc(F * Prec) / Prec.
