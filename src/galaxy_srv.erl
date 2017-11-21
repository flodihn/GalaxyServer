-module(galaxy_srv).
-behaviour(gen_server).

-include("galaxy_defs.hrl").

-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([
    create_galaxy/2,
    create_region/2,
    create_system/3,
    create_planet/3,
    create_moon/3,
    create_asteroid_belt/3,
    create_structure/3]).

-record(state, {implmod, implstate}).

%% ------------------------------------------------------------------
%% gen_server API Function Definitions
%% ------------------------------------------------------------------

start_link(ImplMod) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [ImplMod], []).

%% ------------------------------------------------------------------
%% Galaxy Server API Function Definitions
%% ------------------------------------------------------------------
create_galaxy(Id, Pos) ->
    gen_server:call(?SERVER, {create_galaxy, Id, Pos}).

create_region(GalaxyId, Region) ->
    ok.

create_system(GalaxyId, RegionId, System) ->
    ok.

create_planet(GalaxyId, SystemId, Planet) ->
    ok.

create_moon(GalaxyId, PlanetId, Moon) ->
    ok.

create_asteroid_belt(GalaxyId, LinkId, AsteroidBelt) ->
    ok.

create_structure(GalaxyId, LinkId, Structure) ->
    ok.

get_systems(GalaxyId) ->
    gen_server:call(?SERVER, {get_systems, GalaxyId}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([ImplMod]) ->
    State = ImplMod:init(),
    {ok, #state{implmod=ImplMod, implstate=State}}.

handle_call({create_galaxy, Id, Pos}, _From,
           #state{implmod=ImplMod, implstate=ImplState} = State) ->
    {ok, galaxy_created} = ImplMod:create_galaxy(
        #galaxy{id=Id, pos=Pos}, ImplState),
    galaxy_sim_sup:start_simulation(Id),
    {reply, ok, State};

handle_call({get_systems, GalaxyId}, _From,
           #state{implmod=ImplMod, implstate=ImplState} = State) ->
    {ok, SystemList} = ImplMod:get_systems(GalaxyId, ImplState),
    {reply, {ok, SystemList}, State};



handle_call(Request, _From, State) ->
    error_logger:info_report({unknown_request, Request}),
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

