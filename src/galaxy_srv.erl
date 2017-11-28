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
    create_region/3,
    create_system/5,
    get_systems/1,
    create_planet/5,
    update_planet/1,
    get_planet/2,
    create_moon/3,
    create_asteroid_belt/3,
    create_resource_type/4,
    create_resource_type/5,
    get_resource_type/1,
    create_structure_type/7,
    get_structure_type/1,
    add_resource/6,
    add_structure/4]).

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

create_resource_type(Name, Category, StorageSpace, DisplayName) ->
    create_resource_type(Name, Category, StorageSpace, [], DisplayName).

create_resource_type(Name, Category, StorageSpace, BuildMaterials,
        DisplayName) ->
    gen_server:call(?SERVER, {create_resource_type, Name, Category,
        StorageSpace, BuildMaterials, DisplayName}).

get_resource_type(Name) ->
    gen_server:call(?SERVER, {get_resource_type, Name}).

create_structure_type(Name, Category, Rate, Produces, InputStorageSpace,
        OutputStorageSpace, DisplayName) ->
    gen_server:call(?SERVER, {create_structure_type, Name, Category,
        Rate, Produces, InputStorageSpace, OutputStorageSpace,
        DisplayName}).

get_structure_type(Name) ->
    gen_server:call(?SERVER, {get_structure_type, Name}).

add_resource(GalaxyId, ResourceName, LinkId, LinkType, Capacity, Rate) ->
    gen_server:call(?SERVER, {add_resource, GalaxyId, ResourceName,
        LinkId, LinkType, Capacity, Rate}).

add_structure(GalaxyId, StructureName, LinkId, LinkType) ->
    gen_server:call(?SERVER, {add_structure, GalaxyId, StructureName,
        LinkId, LinkType}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([ImplMod]) ->
    State = ImplMod:init(),
    {ok, #state{implmod=ImplMod, implstate=State}}.

handle_call({create_galaxy, Id, Pos}, _From,
           #state{implmod=ImplMod, implstate=ImplState} = State) ->
    {ok, galaxy_created} = ImplMod:create_galaxy(
        #galaxy{id=Id, pos=Pos, regions=[]}, ImplState),
    error_logger:info_report({starting_simulation, {galaxy_id, Id}}),
    galaxy_sim_sup:start_simulation(Id),
    {reply, ok, State};

handle_call({create_region, GalaxyId, Name, DisplayName}, _From,
           #state{implmod=ImplMod, implstate=ImplState} = State) ->
    {ok, region_created} = ImplMod:create_region(#region{name=Name,
            galaxy_id=GalaxyId, display_name=DisplayName}, ImplState),
    {reply, ok, State};

handle_call({create_system, GalaxyId, Region, Name, Pos, DisplayName},
        _From, #state{implmod=ImplMod, implstate=ImplState} = State) ->
    {ok, system_created} = ImplMod:create_system(#system{name=Name, 
            galaxy_id=GalaxyId, region=Region, pos=Pos,
            display_name=DisplayName}, ImplState),
    {reply, ok, State};

handle_call({create_planet, GalaxyId, System, Name, Orbit, DisplayName},
        _From, #state{implmod=ImplMod, implstate=ImplState} = State) ->
    {ok, planet_created} = ImplMod:create_planet(#planet{name=Name,
        galaxy_id=GalaxyId, system=System, orbit=Orbit,
        display_name=DisplayName}, ImplState),
    {reply, ok, State};

handle_call({update_planet, Planet}, _From, #state{implmod=ImplMod,
        implstate=ImplState} = State) ->
    {ok, planet_updated} = ImplMod:update_planet(Planet, ImplState),
    {reply, ok, State};

handle_call({get_planet, GalaxyId, PlanetName},
        _From, #state{implmod=ImplMod, implstate=ImplState} = State) ->
    {ok, Planet} = ImplMod:get_planet(GalaxyId, PlanetName, ImplState),
    {reply, {ok, Planet}, State};

handle_call({get_systems, GalaxyId}, _From,
           #state{implmod=ImplMod, implstate=ImplState} = State) ->
    {ok, SystemList} = ImplMod:get_systems(GalaxyId, ImplState),
    {reply, {ok, SystemList}, State};

handle_call({create_resource_type, Name, Category, StorageSpace, 
        BuildMaterials, DisplayName}, _From, #state{implmod=ImplMod,
        implstate=ImplState} = State) ->
    {ok, resource_type_created} = ImplMod:create_resource_type(
        #resource_type{name=Name, category=Category,
        storage_space=StorageSpace, build_materials=BuildMaterials,
        display_name=DisplayName}, ImplState),
    {reply, ok, State};

handle_call({get_resource_type, Name}, _From, #state{implmod=ImplMod,
        implstate=ImplState} = State) ->
    {ok, ResourceType} = ImplMod:get_resource_type(Name, ImplState),
    {reply, {ok, ResourceType}, State};

handle_call({create_structure_type, Name, Category, Produces, Rate,
        InputStorageSpace, OutputStorageSpace, DisplayName},
        _From, #state{implmod=ImplMod, implstate=ImplState} = State) ->
    {ok, structure_type_created} = ImplMod:create_structure_type(
        #structure_type{name=Name, category=Category, rate=Rate,
            produces=Produces, input_storage_space=InputStorageSpace,
            output_storage_space=OutputStorageSpace,
            display_name=DisplayName}, ImplState),
    {reply, ok, State};

handle_call({get_structure_type, Name}, _From, #state{implmod=ImplMod,
        implstate=ImplState} = State) ->
    {ok, StructureType} = ImplMod:get_structure_type(Name, ImplState),
    {reply, {ok, StructureType}, State};

handle_call({add_resource, GalaxyId, ResourceName, LinkId, LinkType,
        Amount, Rate}, _From, #state{implmod=ImplMod,
        implstate=ImplState} = State) ->
    case ImplMod:get_resource(GalaxyId, LinkId, LinkType, ResourceName,
            State) of
        {ok, Resource} ->
            {ok, resource_added} = ImplMod:add_resource(
                GalaxyId, LinkId, LinkType, Resource#resource{
                    amount=Resource#resource.amount + Amount},
                ImplState),
            {reply, ok, State};
        {error, resource_not_found} ->
            {ok, resource_added} = ImplMod:add_resource(
                GalaxyId, LinkId, LinkType, #resource{name=ResourceName,
                    amount=Amount},
                ImplState),
            {reply, ok, State};
        {error, planet_not_found} ->
            {reply, {error, link_id_not_found}, State}
    end;

handle_call({add_structure, GalaxyId, StructureName, LinkId, LinkType},
        _From, #state{implmod=ImplMod, implstate=ImplState} = State) ->
    {ok, structure_added} = ImplMod:add_structure(GalaxyId,
        StructureName, LinkId, LinkType, ImplState),
    {reply, ok, State};        

handle_call({get_structures, GalaxyId}, _From, #state{implmod=ImplMod,
        implstate=ImplState} = State) ->
    {ok, AllStructures} = ImplMod:add_structure(GalaxyId, ImplState),
    {reply, {ok, AllStructures}, State};

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

