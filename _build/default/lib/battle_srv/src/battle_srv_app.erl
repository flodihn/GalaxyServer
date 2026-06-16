-module(battle_srv_app).

-behaviour(application).

%% Application callbacks
-export([
    start/0,
    start/2,
    stop/1]).

%% ===================================================================
%% Start function from shell (only for development)
%% ===================================================================
start() ->
    application:start(sasl),
    application:start(battle_srv).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    battle_srv_sup:start_link().

stop(_State) ->
    ok.
