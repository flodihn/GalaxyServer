-module(sws_srv_app).

-behaviour(application).

%% Application callbacks
-export([start/0, start/2, stop/1]).

%% ===================================================================
%% Start function from shell (only for development)
%% ===================================================================
start() ->
    application:start(resource_srv),
	application:start(galaxy_srv),
    application:start(sws_srv),
    observer:start().

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    sws_srv_sup:start_link().

stop(_State) ->
    ok.
