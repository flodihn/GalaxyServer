-module(empire_strategy).

-include("../galaxy_srv/include/galaxy_defs.hrl").
-include("../battle_srv/include/battle_defs.hrl").
-include("faction_defs.hrl").

-ifdef(TEST).
-compile(export_all).
-endif.

-export([
    init/0
    ]).

-export([
	simulate_faction/2
    ]).

init() ->
    mnesia:start(),
    {ok, []}.

simulate_faction(#faction{galaxy_id = GalaxyId,
                          claims = Claims} = Faction, DeltaTime) ->
	{ok, SystemList} = galaxy_srv:get_systems(GalaxyId),
    NonClaimedSystems = get_non_claimed_systems(SystemList, Claims),
    SystemsToClaim = find_systems_to_claim(NonClaimedSystems),
	[dominate_system(System, Faction, DeltaTime) || 
		System <- SystemsToClaim].

get_non_claimed_systems(SystemList, Claims) ->
	AllSystemInClaimsFormat = [{system, X#system.name} || X <- SystemList],
	AllSystemClaims = [{ClaimType, Claim} || {ClaimType, Claim} <- Claims, 
		ClaimType == system],
	% Get all systems not claimed by the faction
	NonClaimedSystems = [X || X <- AllSystemInClaimsFormat,
		lists:member(X, AllSystemClaims) == false],
	NonClaimedSystems.

find_systems_to_claim([]) ->
	[];

find_systems_to_claim(ListOfSystems) ->
	[lists:nth(1, ListOfSystems)].

dominate_system(System, Faction, DeltaTime) ->
    AllEnemyForces = get_enemy_forces_in_system(System, Faction),
    AllEnemyCapabilities = summarize_force_capabilities(AllEnemyForces),
    ok.

get_enemy_forces_in_system(System, Faction) ->
    AllForces = System#system.forces,
    AllEnemyForces = [Force || Force <- AllForces,
        Force#force.faction =/= Faction#faction.name],
    AllEnemyForces.

summarize_force_capabilities(AllEnemyForces) ->
    AllEnemyCapabilities = [Force#force.capabilities || 
        Force <- AllEnemyForces],
    summarize_force_capabilities(AllEnemyCapabilities, dict:new()).

summarize_force_capabilities([], Acc) ->
    dict:to_list(Acc);

summarize_force_capabilities([CapabilityList | Rest], Acc) ->
    UpdatedDict = summarize_capability(CapabilityList, Acc),
    summarize_force_capabilities(Rest, UpdatedDict).

summarize_capability([], Acc) ->
    Acc;

summarize_capability([{ForceClass, Strength}| Rest], Acc) ->
    case dict:find(ForceClass, Acc) of
        {ok, Num} ->
            UpdatedDict = dict:store(ForceClass, Num + Strength, Acc),
            summarize_capability(Rest, UpdatedDict);
        error ->
            UpdatedDict = dict:store(ForceClass, Strength, Acc),
            summarize_capability(Rest, UpdatedDict)
    end.

