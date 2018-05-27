%% @author Cflow
%% @doc @todo Add description to rest_util.
-module(rest_util).

-include("galaxy_defs.hrl").
-include("resource_defs.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([
	response200/1,
	response404/0,
	response500/0,
	galaxies_to_json/1,
	galaxy_to_json/1,
	regions_to_json/1,
	systems_to_json/1,
	resource_types_to_json/1,
	structure_types_to_json/1,
	json_to_record/2]).

response200({struct, Json}) ->
	{200, [], json2:encode({struct, Json})};

response200(Message) ->
	{200, [], json2:encode({struct, [{<<"status">>, <<"ok">>}, {<<"message">>, Message}]})}.

response404() ->
	{404, [], json2:encode({struct, [{<<"status">>, <<"error">>}, {<<"message">>, <<"Not Found">>}]})}.

response500() ->
	{500, [], json2:encode({struct, [
		{<<"status">>, <<"error">>}, {<<"message">>, <<"Internal Server Error">>}]})}.

galaxies_to_json(GalaxyList) ->
	galaxies_to_json(GalaxyList, []).

galaxies_to_json([], Acc) ->
	{struct, [{galaxies, Acc}]};

galaxies_to_json([Galaxy | GalaxyList], Acc) ->
	GalaxyJson = galaxy_id_to_json(Galaxy),
	galaxies_to_json(GalaxyList, [ GalaxyJson | Acc]).
	
galaxy_id_to_json(#galaxy{id=GalaxyId}) ->
	GalaxyId.

galaxy_to_json(Galaxy) ->
	PropList = record_to_proplist(Galaxy),
	JsonPropList = [proplist_values_to_json(Value) || Value <- PropList],
	json2:encode({struct, [{galaxy, {struct, JsonPropList}}]}).

regions_to_json(Regions) ->
	regions_to_json(Regions, []).

regions_to_json([], Acc) ->
	json2:encode({struct, [{regions, {array, Acc}}]});

regions_to_json([Region | Regions], Acc) ->
	PropList = record_to_proplist(Region),
	JsonPropList = [proplist_values_to_json(Value) || Value <- PropList],
	regions_to_json(Regions, [{struct, JsonPropList} | Acc]).

systems_to_json(Systems) ->
	systems_to_json(Systems, []).

systems_to_json([], Acc) ->
	json2:encode({struct, [{systems, {array, Acc}}]});

systems_to_json([System | Systems], Acc) ->
	PropList = record_to_proplist(System),
	JsonPropList = [proplist_values_to_json(Value) || Value <- PropList],
	systems_to_json(Systems, [{struct, JsonPropList} | Acc]).

resource_types_to_json(ResourceTypes) ->
	resource_types_to_json(ResourceTypes, []).

resource_types_to_json([], Acc) ->
	json2:encode({struct, [{resource_types, {array, Acc}}]});

resource_types_to_json([ResourceType | ResourceTypes], Acc) ->
	PropList = record_to_proplist(ResourceType),
	JsonPropList = [proplist_values_to_json(Value) || Value <- PropList],
	resource_types_to_json(ResourceTypes, [{struct, JsonPropList} | Acc]).

structure_types_to_json(StructureTypes) ->
	structure_types_to_json(StructureTypes, []).

structure_types_to_json([], Acc) ->
	json2:encode({struct, [{structure_types, {array, Acc}}]});

structure_types_to_json([StructureType | StructureTypes], Acc) ->
	PropList = record_to_proplist(StructureType),
	JsonPropList = [proplist_values_to_json(Value) || Value <- PropList],
	structure_types_to_json(StructureTypes, [{struct, JsonPropList} | Acc]).

record_to_proplist(#galaxy{} = Rec) ->
	lists:zip(record_info(fields, galaxy), tl(tuple_to_list(Rec)));

record_to_proplist(#region{} = Rec) ->
	[{name, Rec#region.name}, {display_name, Rec#region.display_name}];

record_to_proplist(#system{} = Rec) ->
	lists:zip(record_info(fields, system), tl(tuple_to_list(Rec)));

record_to_proplist(#resource_type{} = Rec) ->
	lists:zip(record_info(fields, resource_type), tl(tuple_to_list(Rec)));

record_to_proplist(#structure_type{} = Rec) ->
	lists:zip(record_info(fields, structure_type), tl(tuple_to_list(Rec)));

record_to_proplist(Rec) ->
	error_logger:error_report({?MODULE, record_to_proplist, 
		unknown_record, Rec}).

proplist_values_to_json({Key, {X, Y, Z}}) ->
	{Key, {struct, [{x, X}, {y, Y}, {z, Z}]}};

proplist_values_to_json({Key, List}) when is_list(List) ->
	case List of
		[] -> 
			{Key, {array, List}};
		_NonEmptyList ->
			case io_lib:latin1_char_list(List) of
				true -> {Key, list_to_binary(List)};
				false -> {Key, {array, List}}
			end
	end;

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
	{ok, Record};

json_to_record(system, Json) ->
	{struct, [{"system", {struct, [
		{"name", Name},
		{"galaxy_id", GalaxyId},
		{"region", Region},
		{"pos", {struct, [{"x", X}, {"y", Y}, {"z", Z}]}},
		{"display_name", DisplayName},
		{"star_type", StarType},
		{"star_size", StarSize},
		{"routes", {array, Routes}},
		{"metadata", MetaData}
	]}}]} = Json,
	Record =  #system{name=list_to_binary(Name), galaxy_id=list_to_binary(GalaxyId),
		region=list_to_binary(Region), pos={X, Y, Z},
		display_name=list_to_binary(DisplayName), star_type=StarType, star_size=StarSize,
		routes=Routes, metadata=list_to_binary(MetaData)},
	{ok, Record};

json_to_record(hyperspace_route, Json) ->
	{struct, [
		{"origin", OriginSystem},
		{"destination", DestinationSystem}
	]} = Json,
	Record =  #hyperspace_route{origin=list_to_binary(OriginSystem),
								destination=list_to_binary(DestinationSystem)},
	{ok, Record}.

