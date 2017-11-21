-module(galaxy_sim).
-behaviour(gen_server).

-include("galaxy_defs.hrl").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% Intenral Function Exports
%% ------------------------------------------------------------------
-export([tick/1]).

%% ------------------------------------------------------------------
%% gen_server API Function Definitions
%% ------------------------------------------------------------------

start_link(GalaxyId) when is_binary(GalaxyId) ->
    GalaxyName = get_galaxy_name(GalaxyId),
    gen_server:start_link({local, GalaxyName}, ?MODULE, [GalaxyId], []).

%% ------------------------------------------------------------------
%% Galaxy Simulation API Function Definitions
%% ------------------------------------------------------------------

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([GalaxyId]) ->
    GalaxyName = get_galaxy_name(GalaxyId),
    spawn_link(?MODULE, tick, [GalaxyName]),   
    {ok, GalaxyId}.

handle_call(tick, _From, GalaxyId) ->
    {reply, ok, GalaxyId};

handle_call(Request, _From, GalaxyId) ->
    error_logger:info_report({unknown_request, Request}),
    {reply, ok, GalaxyId}.

handle_cast(_Msg, GalaxyId) ->
    {noreply, GalaxyId}.

handle_info(_Info, GalaxyId) ->
    {noreply, GalaxyId}.

terminate(_Reason, _GalaxyId) ->
    ok.

code_change(_OldVsn, GalaxyId, _Extra) ->
    {ok, GalaxyId}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

get_galaxy_name(GalaxyId) ->
    list_to_atom(binary_to_list(GalaxyId) ++ "_galaxy").

%% ------------------------------------------------------------------
%% Internal Tick process. 
%% ------------------------------------------------------------------
tick(GalaxyName) ->
    gen_server:call(GalaxyName, tick),
    timer:sleep(2000),
    tick(GalaxyName).
