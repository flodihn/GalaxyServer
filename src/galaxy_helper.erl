-module(galaxy_helper).

-include("galaxy_defs.hrl").

-export([
    init/0
    ]).

init() ->
    galaxy_srv:create_galaxy(<<"test">>, {0, 0, 0}),
    galaxy_srv:create_region(<<"test">>, <<"core">>, <<"Core Worlds">>),
    galaxy_srv:create_system(<<"test">>, <<"core">>, <<"sol">>, {0,0,0}, <<"Sol System">>),
    galaxy_srv:create_planet(<<"test">>, <<"sol">>, <<"terra">>, #orbit{distance=10, angle=360, speed=1}, <<"Earth">>),
    galaxy_srv:create_resource_type(<<"quadanium">>, <<"material">>, 2,
        <<"Quadanium steel">>),
    galaxy_srv:create_resource_type(<<"tie-fighter">>, <<"starfighter">>,
        10, [#resource{name=list_to_binary("quadanium"), amount=10}],
        <<"TIE/LN Fighter">>),
    galaxy_srv:create_structure_type(<<"tie-fighter_facility">>,
        <<"shipyard">>, 3, [<<"tie-fighter">>], 1000, 720,
        <<"TIE-Fighter Production Facility">>),
    galaxy_srv:create_structure_type(<<"quadanium_mine">>,
        <<"mine">>, 10, [<<"quadanium">>], 0, 1000,
        <<"Quadanium Mine">>),

    galaxy_srv:add_structure(<<"test">>, <<"quadanium_mine">>, <<"terra">>,
        planet),
    galaxy_srv:add_structure(<<"test">>, <<"quadanium_mine">>, <<"terra">>,
        planet),
    galaxy_srv:add_structure(<<"test">>, <<"tie-fighter_facility">>,
        <<"terra">>, planet).
