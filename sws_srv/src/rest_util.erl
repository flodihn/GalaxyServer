%% @author Cflow
%% @doc @todo Add description to rest_util.
-module(rest_util).


-include("galaxy_defs.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([galaxy_list_to_json/1, galaxy_to_json/1]).

galaxy_list_to_json(GalaxyList) ->
	galaxy_list_to_json(GalaxyList, []).

galaxy_list_to_json([], Acc) ->
	{struct, [{galaxies, Acc}]};

galaxy_list_to_json([Galaxy | GalaxyList], Acc) ->
	GalaxyJson = galaxy_to_json(Galaxy),
	galaxy_list_to_json(GalaxyList, [ GalaxyJson | Acc]).
	
%% ====================================================================
%% Internal functions
%% ====================================================================

galaxy_to_json(#galaxy{id=GalaxyId, pos={X, Y, Z}, regions=_Regions}) ->
	GalaxyId.
