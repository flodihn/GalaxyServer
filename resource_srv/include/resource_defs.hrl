% Mnesia table records

-define(RESOURCE_RESOURCE_EVENT_MANAGER, resource_event_manager).

-record(structure_type, {
    name,
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
    category,
    storage_space = 1,
    display_name,
    build_materials = [],
    build_time = 0,
    metadata}).

% Non mnesia table records

-record(resource, {
    name,
    amount}).

-record(queue_item, {
    resource,
    finish_time}).

-record(structure, {
    uid,
    name,
    build_queue = [],
    output_resources = [],
    input_resources = [],
    output_storage_space = 0,
    input_storage_space = 0
    }).
