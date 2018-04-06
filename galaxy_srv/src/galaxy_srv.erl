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
    start_simulation/0,
    set_simulation_callback/1,
    create_galaxy/2,
    get_galaxies/0,
    create_region/3,
    create_system/5,
    get_systems/1,
    get_system/2,
    create_planet/5,
    update_planet/1,
    get_planet/2,
    create_moon/3,
    create_asteroid_belt/3,
    add_structure/4]).

-record(state, {simulation_callback, implmod, implstate}).

%% ------------------------------------------------------------------
%% gen_server API Function Definitions
%% ------------------------------------------------------------------

start_link(ImplMod) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [ImplMod], []).

%% ------------------------------------------------------------------
%% Galaxy Server API Function Definitions
%% ------------------------------------------------------------------
start_simulation() ->
    gen_server:cast(?SERVER, start_simulation).

set_simulation_callback(SimCallback) ->
    gen_server:call(?SERVER, {set_simulation_callback, SimCallback}).

create_galaxy(Id, Pos) when is_binary(Id) ->
    gen_server:call(?SERVER, {create_galaxy, Id, Pos}).

get_galaxies() ->
    gen_server:call(?SERVER, {get_galaxies}).

create_region(GalaxyId, Name, DisplayName) ->
    gen_server:call(?SERVER, {create_region, GalaxyId, Name, DisplayName}).

create_system(GalaxyId, Region, Name, Pos, DisplayName) ->
    gen_server:call(?SERVER, {create_system, GalaxyId, Region, Name, Pos,
        DisplayName}).

create_planet(GalaxyId, System, Name, Orbit, DisplayName) ->
    gen_server:call(?SERVER, {create_planet, GalaxyId, System, Name, Orbit,
        DisplayName}).

update_planet(Planet) ->
    gen_server:call(?SERVER, {update_planet, Planet}).

get_planet(GalaxyId, PlanetName) ->
    gen_server:call(?SERVER, {get_planet, GalaxyId, PlanetName}).

create_moon(GalaxyId, PlanetId, Moon) ->
    ok.

create_asteroid_belt(GalaxyId, LinkId, AsteroidBelt) ->
    ok.

get_systems(GalaxyId) ->
    gen_server:call(?SERVER, {get_systems, GalaxyId}).

get_system(GalaxyId, SystemName) ->
    gen_server:call(?SERVER, {get_system, GalaxyId, SystemName}).

create_resource_type(Name, Category, StorageSpace, BuildTime,
        DisplayName) ->
    create_resource_type(Name, Category, StorageSpace, [], BuildTime,
        DisplayName).

create_resource_type(Name, Category, StorageSpace, BuildMaterials,
        BuildTime, DisplayName) ->
    gen_server:call(?SERVER, {create_resource_type, Name, Category,
        StorageSpace, BuildMaterials, BuildTime, DisplayName}).

get_resource_type(Name) ->
    gen_server:call(?SERVER, {get_resource_type, Name}).

create_structure_type(Name, Category, ProductionRate, Produces,
        InputStorageSpace, OutputStorageSpace, DisplayName) ->
    gen_server:call(?SERVER, {create_structure_type, Name, Category,
        ProductionRate, Produces, InputStorageSpace, OutputStorageSpace,
        DisplayName}).

get_structure_type(Name) ->
    gen_server:call(?SERVER, {get_structure_type, Name}).

add_structure(GalaxyId, Structure, LinkId, LinkType) ->
    gen_server:call(?SERVER, {add_structure, GalaxyId, Structure,
        LinkId, LinkType}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([ImplMod]) ->
    State = ImplMod:init(),
    {ok, #state{implmod=ImplMod, implstate=State}}.

handle_call({set_simulation_callback, SimCallback}, _From, State) ->
    {reply, ok, State#state{simulation_callback = SimCallback}};

handle_call({create_galaxy, Id, Pos}, _From,
           #state{implmod=ImplMod, implstate=ImplState} = State) ->
    {ok, galaxy_created} = ImplMod:create_galaxy(
        #galaxy{id=Id, pos=Pos, regions=[]}, ImplState),
    error_logger:info_report({starting_simulation, {galaxy_id, Id}}),
    {reply, ok, State};

handle_call({get_galaxies}, _From,
           #state{implmod=ImplMod, implstate=ImplState} = State) ->
    {ok, GalaxyList} = ImplMod:get_galaxies(ImplState),
    {reply, {ok, GalaxyList}, State};

handle_call({create_region, GalaxyId, Name, DisplayName}, _From,
           #state{implmod=ImplMod, implstate=ImplState} = State) ->
    {ok, region_created} = ImplMod:create_region(#region{name=Name,
            galaxy_id=GalaxyId, display_name=DisplayName}, ImplState),
    {reply, ok, State};

handle_call({create_system, GalaxyId, Region, Name, Pos, DisplayName},
        _From, #state{implmod=ImplMod, implstate=ImplState} = State) ->
    System = #system{
            name = Name, 
            galaxy_id = GalaxyId,
            region = Region,
            pos = Pos,
            display_name = DisplayName},
    {ok, system_created} = ImplMod:create_system(System, ImplState),
    %galaxy_sim:simulate_system(System),
    {reply, ok, State};

handle_call({create_planet, GalaxyId, System, Name, Orbit, DisplayName},
        _From, #state{implmod=ImplMod, implstate=ImplState} = State) ->
    {ok, planet_created} = ImplMod:create_planet(#planet{name=Name,
        galaxy_id=GalaxyId, system=System, orbit=Orbit,
        display_name=DisplayName}, ImplState),
    {reply, {ok, planet_created}, State};

handle_call({update_planet, Planet}, _From, #state{implmod=ImplMod,
        implstate=ImplState} = State) ->
    {ok, planet_updated} = ImplMod:update_planet(Planet, ImplState),
    {reply, {ok, planet_updated}, State};

handle_call({get_planet, GalaxyId, PlanetName},
        _From, #state{implmod=ImplMod, implstate=ImplState} = State) ->
    {ok, Planet} = ImplMod:get_planet(GalaxyId, PlanetName, ImplState),
    {reply, {ok, Planet}, State};

handle_call({get_systems, GalaxyId}, _From,
           #state{implmod=ImplMod, implstate=ImplState} = State) ->
    {ok, SystemList} = ImplMod:get_systems(GalaxyId, ImplState),
    {reply, {ok, SystemList}, State};

handle_call({get_system, GalaxyId, SystemName}, _From,
           #state{implmod=ImplMod, implstate=ImplState} = State) ->
    {ok, System} = ImplMod:get_system(GalaxyId, SystemName, ImplState),
    {reply, {ok, System}, State};

handle_call({create_resource_type, Name, Category, StorageSpace, 
        BuildMaterials, BuildTime, DisplayName}, _From,
        #state{implmod=ImplMod, implstate=ImplState} = State) ->
    {ok, resource_type_created} = ImplMod:create_resource_type(
        #resource_type{name = Name, category = Category,
        storage_space = StorageSpace, build_materials = BuildMaterials,
        build_time = BuildTime, display_name=DisplayName}, ImplState),
    {reply, ok, State};

handle_call({get_resource_type, Name}, _From, #state{implmod=ImplMod,
        implstate=ImplState} = State) ->
    {ok, ResourceType} = ImplMod:get_resource_type(Name, ImplState),
    {reply, {ok, ResourceType}, State};

handle_call({create_structure_type, Name, Category, ProductionRate,
        Produces, InputStorageSpace, OutputStorageSpace, DisplayName},
        _From, #state{implmod=ImplMod, implstate=ImplState} = State) ->
    {ok, structure_type_created} = ImplMod:create_structure_type(
        #structure_type{name=Name, category=Category,
            production_rate = ProductionRate, produces = Produces,
            input_storage_space = InputStorageSpace,
            output_storage_space = OutputStorageSpace,
            display_name = DisplayName}, ImplState),
    {reply, ok, State};

handle_call({get_structure_type, Name}, _From, #state{implmod=ImplMod,
        implstate=ImplState} = State) ->
    {ok, StructureType} = ImplMod:get_structure_type(Name, ImplState),
    {reply, {ok, StructureType}, State};

handle_call({add_structure, GalaxyId, Structure, LinkId, planet},
        _From, #state{implmod=ImplMod, implstate=ImplState} = State) ->
    {ok, structure_added} = ImplMod:add_structure(GalaxyId,
        Structure, LinkId, planet, ImplState),
    {reply, {ok, structure_added}, State};        

handle_call({add_structure, GalaxyId, Structure, LinkId, UnknownLinkType},
        _From, #state{implmod=ImplMod, implstate=ImplState} = State) ->
    {reply, {error, unknown_link_type}, State};        

handle_call({get_structures, GalaxyId}, _From, #state{implmod=ImplMod,
        implstate=ImplState} = State) ->
    {ok, AllStructures} = ImplMod:add_structure(GalaxyId, ImplState),
    {reply, {ok, AllStructures}, State};

handle_call(Request, _From, State) ->
    error_logger:info_report({unknown_request, Request}),
    {reply, ok, State}.

handle_cast(start_simulation,
        #state{simulation_callback = undefined} = State) ->
    error_logger:error_report({?MODULE, start_simulation,
        {error, simmod_undefined}, simulation_not_started}), 
    {noreply, State};

handle_cast(start_simulation,
        #state{simulation_callback = SimMod, implmod = ImplMod,
        implstate = ImplState} = State) ->
    error_logger:info_report({starting_simulation}),
    start_galaxy_simulations(State),
    {noreply, State};

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
start_galaxy_simulations(#state{simulation_callback = SimCallback,
        implmod = ImplMod, implstate = ImplState}) ->
    % This is not really good, but putting the wait_for_tables it into the
    % ImplMod:init did not work.
    timer:sleep(1000),
    {ok, GalaxyList} = ImplMod:get_galaxies(ImplState),
    [galaxy_sim_sup:start_simulation(Galaxy#galaxy.id, SimCallback) || 
        Galaxy <- GalaxyList].
