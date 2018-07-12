% Mnesia table records

-record(fleet, {
    name,
    galaxy_id,
	faction_group,
	primary_wing,
	secondary_wings=[],
    metadata
    }).

-record(wing, {
    name,
    galaxy_id,
	faction_group,
	primary_squadron,
	secondary_squadrons=[],
    metadata
    }).

-record(squadron, {
    name,
    galaxy_id,
	faction_group,
    metadata
    }).

-record(pilot, {
	id,
	firstname,
	lastname,
	effiency_rating,
	kills,
	deaths,
	}).
