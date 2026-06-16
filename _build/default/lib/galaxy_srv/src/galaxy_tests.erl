-module(galaxy_tests).

-include_lib("eunit/include/eunit.hrl").

-include("galaxy_defs.hrl").

test_setup() ->
    %meck:new(galaxy_srv, []),
    %meck:expect(galaxy_srv, get_structure_type,
    %    fun(StructureName) -> 
    %        case StructureName of
    %            <<"test_mine">> ->
    %                {ok, ?STRUCTURE_TYPE_MINE};
    %            <<"test_factory">> -> 
    %                {ok, ?STRUCTURE_TYPE_FACTORY};
    %            <<"small_shipyard">> ->
    %                {ok, ?STRUCTURE_TYPE_SMALL_SHIPYARD}
    %        end
    %    end),
    ok.

test_teardown(_Arg) ->
    %meck:unload(galaxy_srv).
    ok.

fixture_test_() ->
    {
        setup,
            fun test_setup/0, fun test_teardown/1,
            [
                %{"Simulate structure",
                %    ?_test(test_simulate_structure())}
            ]
        }.

%test_simulate_structure() ->
%   Structure = #structure{
%        name = <<"small_shipyard">>,
%        output_resources = [],
%        input_resources = [],
%        output_storage_space = 0,
%        input_storage_space = 0},
%    ExpectedStructure = #structure{
%            name = <<"small_shipard">>,
%            output_resources = [
%                #resource{
%                    name = <<"small_space_ship">>,
%                    amount = 1}
%            ],
%            input_resources = [
%                #resource{
%                    name = <<"metal">>,
%                    amount = 10}
%            ],
%            input_storage_space = 0,
%            output_storage_space = 10},
%    DeltaTime = 1.0,
%    ?assertEqual(
%        {ok, ExpectedStructure},
%        galaxy_structure_util:simulate_structure(Structure, DeltaTime)).
