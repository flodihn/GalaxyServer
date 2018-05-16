%% @author Cflow
%% @doc @todo Add description to rest_callback.
-module(rest_callback).

-include("galaxy_defs.hrl").
-include("../deps/elli/include/elli.hrl").
-behaviour(elli_handler).

-export([handle/2, handle/3, handle_event/3]).

-define(DEFAULT_REQUEST_HEADERS, [
    {<<"Content-Type">>, <<"application/json">>},
	{<<"Access-Control-Allow-Origin">>, <<"*">>},
	{<<"Access-Control-Allow-Methods">>, <<"GET, PUT, DELETE">>}]).

handle(Req, _Args) ->
    %% Delegate to our handler function
    handle(Req#req.method, elli_request:path(Req), Req).

handle('GET',[<<"galaxies">>], _Req) ->
	{ok, GalaxyList} = galaxy_srv:get_galaxies(),
	JsonGalaxyList = rest_util:galaxies_to_json(GalaxyList),
    {200, [], json2:encode(JsonGalaxyList)};

handle('GET',[<<"galaxies">>, GalaxyId], _Req) ->
    case galaxy_srv:get_galaxy(GalaxyId) of 
        {ok, #galaxy{} = Galaxy} ->
            Response = rest_util:galaxy_to_json(Galaxy),
            {200, [], Response};
        {error, not_found} ->
            rest_util:response404();
        {error, Reason} ->
            error_logger:error_report(Reason),
            rest_util:response500()
    end;

handle('GET',[<<"galaxies">>, GalaxyId, <<"regions">>], _Req) ->
    case galaxy_srv:get_regions(GalaxyId) of 
        {ok, Regions} ->
            Response = rest_util:regions_to_json(Regions),
			{200, [], Response};
        {error, not_found} ->
            rest_util:response404();
        {error, Reason} ->
            error_logger:error_report(Reason),
            rest_util:response500()
    end;

handle('GET',[<<"galaxies">>, GalaxyId, <<"systems">>], _Req) ->
    case galaxy_srv:get_systems(GalaxyId) of 
        {ok, Systems} ->
            Response = rest_util:systems_to_json(Systems),
			{200, [], Response};
        {error, not_found} ->
            rest_util:response404();
        {error, Reason} ->
            error_logger:error_report(Reason),
            rest_util:response500()
    end;

handle('PUT',[<<"galaxies">>, GalaxyId], Req) ->
    Body = elli_request:body(Req),
    error_logger:info_report({body, Body}),
    {ok, JsonBody} = json2:decode_string(binary_to_list(Body)),
    {ok, Galaxy} = rest_util:json_to_record(galaxy, JsonBody),
    case galaxy_srv:update_galaxy(Galaxy) of
        {error, not_found} ->
            rest_util:response404();
        {ok, galaxy_updated} ->
            rest_util:response200(<<"galaxy_updated">>)
    end;
 
 handle('POST',[<<"galaxies">>, GalaxyId], _Req) ->
	{200, [], json2:encode({struct, [{<<"ok">>, <<"galaxy">>}]})};

 handle('POST',[<<"galaxies">>, GalaxyId, <<"systems">>], Req) ->
    Body = elli_request:body(Req),
	error_logger:info_report({body, Body}),
    {ok, JsonBody} = json2:decode_string(binary_to_list(Body)),
    {ok, System} = rest_util:json_to_record(system, JsonBody),
	
    case galaxy_srv:create_system(System) of
        {ok, system_created} ->
            rest_util:response200(<<"system_created">>)
    end;
   
handle('DELETE',[<<"galaxies">>, GalaxyId], _Req) ->
    %% Reply with a normal response. 'ok' can be used instead of '200'
    %% to signal success.
	%case galaxy_srv:destroy_galaxy(GalaxyId) of
%		{ok, galaxy_destroyed} ->
%			{200, [], json2:encode({struct, [{<<"ok">>, <<"galaxy_destroyed">>}]})};
%		{error, galaxy_not_found} ->
%			{404, [], json2:encode({struct, [{<<"ok">>, <<"galaxy_destroyed">>}]})};
%		{error, Reason} ->
%			error_logger:error_report(Reason),
%			{500, [], json2:encode({struct, [{<<"error">>, <<"internal_server_error">>}]})}
	%end;
	{200, [], json2:encode({struct, [{<<"ok">>, <<"galaxy_destroyed">>}]})};

handle('DELETE',[<<"galaxies">>, GalaxyId, <<"systems">>, SystemName], _Req) ->
	case galaxy_srv:remove_system(GalaxyId, SystemName) of
		{ok, system_removed} ->
			rest_util:response200(<<"system_removed">>);
		{error, system_not_found} ->
			rest_util:response404();
		{error, Reason} ->
			error_logger:error_report(Reason),
			{500, [], json2:encode({struct, [{<<"error">>, <<"internal_server_error">>}]})}
	end;

handle(_, _, _Req) ->
    rest_util:response404().

%% @doc: Handle request events, like request completed, exception
%% thrown, client timeout, etc. Must return 'ok'.
handle_event(_Event, _Data, _Args) ->
    ok.
