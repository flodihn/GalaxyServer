-module(galaxy_helper).

-include("galaxy_defs.hrl").

-export([
    init/0
    ]).

init() ->
    galaxy_srv:create_galaxy(
        <<"test">>,
        {0, 0, 0}),

    galaxy_srv:create_region(
        <<"test">>,
        <<"core">>,
        <<"Core Worlds">>),

    galaxy_srv:create_resource_type(
        <<"quadanium">>,
        <<"material">>,
        2,
        0,
        <<"Quadanium steel">>),
    galaxy_srv:create_resource_type(
        <<"tie-fighter">>,
        <<"starfighter">>,
        10,
        [#resource{name=list_to_binary("quadanium"), amount=10}],
        2,
        <<"TIE/LN Fighter">>),
    galaxy_srv:create_resource_type(
        <<"small_spaceship">>,
        <<"spaceship">>,
        10,
        [#resource{name=list_to_binary("metal"), amount=10}],
        2,
        <<"Small spaceship">>),
 
    galaxy_srv:create_structure_type(
        <<"quadanium_mine">>,
        <<"mine">>,
        1,
        [#resource{name = list_to_binary("quadanium"), amount = 10}],
        0,
        1000,
        <<"Quadanium Mine">>),

    galaxy_srv:create_structure_type(
        <<"small_shipyard">>,
        <<"shipyard">>,
        3,
        [#resource{
            name = list_to_binary("small_spaceship"),
            amount = 1}],
        1000,
        720,
        <<"Small Shipyard">>),

    galaxy_srv:create_structure_type(
        <<"small_shipyard">>,
        <<"shipyard">>,
        3,
        [#resource{name = list_to_binary("small_spaceship"), amount = 1}],
        1000,
        1000,
        <<"Small Shipyard">>),

    create_sol_system(),
    create_alpha_centauri_system(),
    ok.

create_sol_system() ->
    galaxy_srv:create_system(
        <<"test">>,
        <<"core">>,
        <<"sol">>,
        {0,0,0},
        <<"Sol System">>),
    galaxy_srv:create_planet(
        <<"test">>,
        <<"sol">>,
        <<"terra">>,
        #orbit{distance=10, angle=360, speed=1}, <<"Earth">>),
    galaxy_srv:add_structure(
        <<"test">>,
        #structure{name=list_to_binary("quadanium_mine")},
        <<"terra">>,
        planet),
    ok.

create_alpha_centauri_system() ->
   galaxy_srv:create_system(
        <<"test">>,
        <<"core">>,
        <<"alpha_centauri">>,
        {1.3,0, 6.2},
        <<"Alpha Centauri">>),
    ok.
