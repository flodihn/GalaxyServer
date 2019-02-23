-module(force_srv_app).

-behaviour(application).

%% Application callbacks
-export([start/0, start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start() ->
    observer:start(),
    application:start(force_srv).

start(_StartType, _StartArgs) ->
    force_srv_sup:start_link().

stop(_State) ->
    ok.
