% Mnesia table records

-define(RESOURCE_RESOURCE_EVENT_MANAGER, resource_event_manager).

-record(structure_type, {
    name,
    galaxy_id,
    category,
    production_rate = 1,
    produces = [],
    input_storage_space = 1000,
    output_storage_space = 1000,
    output_queue = [],
    display_name,
    metadata
    }).

-record(resource_type, {
    name,
    galaxy_id,
    category,
    storage_space = 1,
    display_name,
    build_materials = [],
    build_time = 0,
    type,
    metadata}).

% Non mnesia table records

-record(resource, {
    name,
    galaxy_id,
    amount}).

-record(queue_item, {
    resource,
    galaxy_id,
    finish_time}).

-record(structure, {
    uid,
    galaxy_id,
    name,
    build_queue = [],
    output_resources = [],
    input_resources = [],
    output_storage_space = 0,
    input_storage_space = 0
    }).
