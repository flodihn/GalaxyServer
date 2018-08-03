% Mnesia table records

-record(force, {
    id,
    galaxy_id,
    display_name,
	simulation_callbacks=[],
	resources=[],
	capabilities=[],
	strength=0.0,
    faction,
    faction_group,
    metadata
    }).

-record(weapon_type, { 
	name,
	galaxy_id,
	display_name,
	strong_vs=[],
	weak_vs=[],
	base_strength,
    metadata}).

-record(force_class, {
    name,
    galaxy_id,
    display_name,
    metadata}).

-record(force_model, {
    name,
    galaxy_id,
    display_name,
    class,
    weapons=[],
    metadata}).
