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
    create_planet/5,
    create_moon/3,
    create_asteroid_belt/3,
    create_resource_type/4,
    create_resource_type/5,
    create_structure_type/7,
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

create_structure_type(Name, Category, Rate, Produces,
        InputStorageSpace, OutputStorageSpace, DisplayName) ->
    gen_server:call(?SERVER, {create_structure_type, Name, Category, Rate,
        Produces, InputStorageSpace, OutputStorageSpace,
        DisplayName}).

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

handle_call({get_systems, GalaxyId}, _From,
           #state{implmod=ImplMod, implstate=ImplState} = State) ->
    {ok, SystemList} = ImplMod:get_systems(GalaxyId, ImplState),
    {reply, {ok, SystemList}, State};

handle_call({create_resource_type, Name, Category, StorageSpace, 
        DisplayName, BuildMaterials}, _From, #state{implmod=ImplMod,
        implstate=ImplState} = State) ->
    {ok, resource_type_created} = ImplMod:create_resource_type(
        #resource_type{name=Name, category=Category,
        storage_space=StorageSpace, display_name=DisplayName,
        build_materials=BuildMaterials}, ImplState),
    {reply, ok, State};

handle_call({create_structure_type, Name, Category, Rate, Produces, 
        InputStorageSpace, OutputStorageSpace, DisplayName},
        _From, #state{implmod=ImplMod, implstate=ImplState} = State) ->
    {ok, structure_type_created} = ImplMod:create_structure_type(
        #structure_type{name=Name, category=Category, rate=Rate, 
            produces=Produces, input_storage_space=InputStorageSpace,
            output_storage_space=OutputStorageSpace,
            display_name=DisplayName}, ImplState),
    {reply, ok, State};

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

