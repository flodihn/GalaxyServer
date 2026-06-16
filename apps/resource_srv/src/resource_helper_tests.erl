-module(resource_helper_tests).

-include_lib("eunit/include/eunit.hrl").

-include("resource_defs.hrl").

-define(GALAXY_ID, <<"resource_helper_test">>).

test_setup() ->
    ok.

test_teardown(_Arg) ->
    ok.

fixture_test_() ->
    {setup,
        fun test_setup/0, fun test_teardown/1, [
            {"Add To Empty Resource List",
                ?_test(test_add_to_empty_resource_list())},
            {"Add To Existing Resource List",
                ?_test(test_add_to_existing_resource_list())},
            {"Remove From Existing Resource List",
                ?_test(test_remove_from_existing_resource_list())},
            {"Remove From empty Resource List",
                ?_test(test_remove_from_empty_resource_list())},
            {"Remove From Zero Value  Resource List",
                ?_test(test_remove_from_resource_list_when_zero())},
            {"Remove From Lower Than Zero Value  Resource List",
                ?_test(test_remove_from_resource_list_lower_than_zero())}
        ]
    }.

test_add_to_empty_resource_list() ->
    ResourceList = [],
    Resource = #resource{name = <<"foo">>, amount = 99},
    Expected = {ok, [#resource{name = <<"foo">>, amount = 99}]},
    ?assertEqual(
        Expected,
        resource_helper:add_to_resource_list(Resource, ResourceList)
    ).

test_add_to_existing_resource_list() ->
    ResourceList = [#resource{name = <<"foo">>, amount = 99}],
    Resource = #resource{name = <<"foo">>, amount = 3},
    Expected = {ok, [#resource{name = <<"foo">>, amount = 102}]},
    ?assertEqual(
        Expected,
        resource_helper:add_to_resource_list(Resource, ResourceList)
    ).

test_remove_from_existing_resource_list() ->
    ResourceList = [#resource{name = <<"foo">>, amount = 99}],
    Resource = #resource{name = <<"foo">>, amount = 3},
    Expected = {ok, [#resource{name = <<"foo">>, amount = 96}]},
    ?assertEqual(
        Expected,
        resource_helper:remove_from_resource_list(Resource,
                                                  ResourceList)
    ).

test_remove_from_empty_resource_list() ->
    ResourceList = [],
    Resource = #resource{name = <<"foo">>, amount = 3},
    Expected = {ok, []},
    ?assertEqual(
        Expected,
        resource_helper:remove_from_resource_list(Resource,
                                                  ResourceList)
    ).

test_remove_from_resource_list_when_zero() ->
    ResourceList = [#resource{name = <<"foo">>, amount = 99}],
    Resource = #resource{name = <<"foo">>, amount = 99},
    Expected = {ok, []},
    ?assertEqual(
        Expected,
        resource_helper:remove_from_resource_list(Resource,
                                                  ResourceList)
    ).

test_remove_from_resource_list_lower_than_zero() ->
    ResourceList = [#resource{name = <<"foo">>, amount = 10}],
    Resource = #resource{name = <<"foo">>, amount = 20},
    Expected = {ok, []},
    ?assertEqual(
        Expected,
        resource_helper:remove_from_resource_list(Resource,
                                                  ResourceList)
    ).
