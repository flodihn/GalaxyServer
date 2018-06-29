-module(empire_strategy).

-include("../galaxy_srv/include/galaxy_defs.hrl").
-include("faction_defs.hrl").

-export([
    init/0
    ]).

-export([
	simulate_faction/2
    ]).

init() ->
    mnesia:start(),
    {ok, []}.

simulate_faction(#faction{galaxy_id=GalaxyId, claims=Claims} = Faction,
		DeltaTime) ->
	{ok, SystemList} = galaxy_srv:get_systems(GalaxyId),
	AllSystemInClaimsFormat = [{system, X#system.name} || X <- SystemList],
	AllSystemClaims = [{ClaimType, Claim} || {ClaimType, Claim} <- Claims, 
		ClaimType == system],
	% Get all systems not claimed by the faction
	NonClaimedSystems = [X || X <- AllSystemInClaimsFormat,
		lists:member(X, AllSystemClaims) == false],
	SystemsToClaim = find_systems_to_claim(NonClaimedSystems),
	[dominate_system(System, Faction, DeltaTime) || 
		System <- SystemsToClaim].

find_systems_to_claim([]) ->
	[];

find_systems_to_claim(ListOfSystems) ->
	[lists:nth(1, ListOfSystems)].

dominate_system(System, Faction, DeltaTime) ->
	error_logger:info_report({?MODULE, dominate_system, Faction,
			System, DeltaTime}).
