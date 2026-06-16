-module(holonet_srv_app).

-behaviour(application).

-export([start/0, start/2, stop/1]).

start() ->
    mnesia:start(),
    application:start(holonet_srv).

start(_StartType, _StartArgs) ->
    mnesia:start(),
    holonet_srv_sup:start_link().

stop(_State) ->
    ok.
