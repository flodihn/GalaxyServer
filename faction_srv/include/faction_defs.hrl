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
    id,
    galaxy_id,
    display_name,
    faction,
    founder,
    admins=[],
    npcs=[],
    players=[],
    modules=[],
    created,
    metadata}).

-record(faction_claim, {
    claim,
    galaxy_id,
    faction,
    metadata}).
