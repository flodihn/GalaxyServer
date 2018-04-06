-module(galaxy_srv_app).

-behaviour(application).

%% Application callbacks
-export([
    start/0,
    start/2,
    stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start() ->
    application:start(sasl),
    application:start(galaxy_srv),
    observer:start().

start(_StartType, _StartArgs) ->
    galaxy_srv_sup:start_link().

stop(_State) ->
    ok.
