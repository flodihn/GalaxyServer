%% @author Cflow
%% @doc @todo Add description to rest_util.
-module(rest_util).

-include("galaxy_defs.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([
	response200/1,
	response404/0,
	response500/0,
	galaxy_list_to_json/1,
	galaxy_to_json/1,
	json_to_record/2]).

response200({struct, Json}) ->
	{200, [], json2:encode({struct, Json})};

response200(Message) ->
	{200, [], json2:encode({struct, [{<<"ok">>, Message}]})}.

response404() ->
	{404, [], json2:encode({struct, [{<<"error">>, <<"Not Found">>}]})}.

response500() ->
	{500, [], json2:encode({struct, [
		{<<"error">>, <<"Internal Server Error">>}]})}.

galaxy_list_to_json(GalaxyList) ->
	galaxy_list_to_json(GalaxyList, []).

galaxy_list_to_json([], Acc) ->
	{struct, [{galaxies, Acc}]};

galaxy_list_to_json([Galaxy | GalaxyList], Acc) ->
	GalaxyJson = galaxy_id_to_json(Galaxy),
	galaxy_list_to_json(GalaxyList, [ GalaxyJson | Acc]).
	
%% ====================================================================
%% Internal functions
%% ====================================================================

galaxy_id_to_json(#galaxy{id=GalaxyId}) ->
	GalaxyId.

galaxy_to_json(Galaxy) ->
	PropList = record_to_proplist(Galaxy),
	JsonPropList = [proplist_values_to_json(Value) || Value <- PropList],
	json2:encode({struct, [{galaxy, {struct, JsonPropList}}]}).

record_to_proplist(#galaxy{} = Rec) ->
  lists:zip(record_info(fields, galaxy), tl(tuple_to_list(Rec)));

record_to_proplist(Rec) ->
	error_logger:error_report({?MODULE, record_to_proplist, 
		unknown_record, Rec}).

proplist_values_to_json({Key, {X, Y, Z}}) ->
	{Key, {struct, [{x, X}, {y, Y}, {z, Z}]}};

proplist_values_to_json({Key, List}) when is_list(List) ->
	{Key, {array, List}};

proplist_values_to_json({Key, Value}) ->
	{Key, Value}.

json_to_record(galaxy, Json) ->
	{struct, [{"galaxy", {struct, [
		{"id", Id},
		{"pos", {struct, [{"x", X}, {"y", Y}, {"z", Z}]}},
		{"seed", Seed},
		{"num_arms", NumArms},
		{"num_stars", NumStars},
		{"stars_in_core", StarsInCore},
		{"core_size", CoreSize},
		{"spin", Spin},
		{"arm_spread", ArmSpread},
		{"thickness", Thickness}
	]}}]} = Json,
	Record =  #galaxy{id=list_to_binary(Id), pos={X, Y, Z},
		seed=Seed, num_arms=NumArms, num_stars=NumStars,
		stars_in_core=StarsInCore, core_size=CoreSize, spin=Spin,
		arm_spread=ArmSpread, thickness=Thickness},
	{ok, Record}.

