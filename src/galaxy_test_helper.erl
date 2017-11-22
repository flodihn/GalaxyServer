-module(galaxy_test_helper).

-include("galaxy_defs.hrl").

-export([
    create_test_galaxy/0
    ]).

create_test_galaxy() ->
    galaxy_srv:create_galaxy(<<"test">>, {0, 0, 0}),
    galaxy_srv:create_region(<<"test">>, <<"core">>, <<"Core Worlds">>),
    galaxy_srv:create_system(<<"test">>, <<"core">>, <<"sol">>, {0,0,0}, <<"Sol System">>),
    galaxy_srv:create_planet(<<"test">>, <<"sol">>, <<"terra">>, #orbit{distance=10, angle=360, speed=1}, <<"Earth">>).
 
