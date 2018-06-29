% Mnesia table records

-record(faction, {
    name,
    galaxy_id,
    display_name,
	claims=[],
	strategy_modules=[],
    metadata
    }).

-record(faction_group, {
    name,
    galaxy_id,
    faction,
    founder,
    admins,
    members,
    modules,
    created,
    metadata}).

-record(faction_claim, {
    claim,
    galaxy_id,
    faction,
    metadata}).

% Non mnesia table records
