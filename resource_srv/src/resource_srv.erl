-module(resource_srv).
-behaviour(gen_server).

-include("resource_defs.hrl").

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
    add_event_handler/1,
    create_resource_type/5,
    create_resource_type/6,
    get_resource_type/1,
    create_structure_type/7,
    get_structure_type/1]).

-record(state, {implmod, implstate}).

%% ------------------------------------------------------------------
%% gen_server API Function Definitions
%% ------------------------------------------------------------------

start_link(ImplMod) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [ImplMod], []).

%% ------------------------------------------------------------------
%% Galaxy Server API Function Definitions
%% ------------------------------------------------------------------
add_event_handler(EventHandler) ->
    gen_event:add_handler(?RESOURCE_RESOURCE_EVENT_MANAGER, EventHandler).

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

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([ImplMod]) ->
    State = ImplMod:init(),
    {ok, #state{implmod=ImplMod, implstate=State}}.

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
    case ImplMod:get_resource_type(Name, ImplState) of
        {ok, ResourceType} ->
            {reply, {ok, ResourceType}, State};
        {error, not_found} ->
            {reply, {error, not_found}, State}
    end;

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
