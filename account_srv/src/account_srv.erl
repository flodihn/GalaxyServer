-module(account_srv).
-behaviour(gen_server).

% External exports
-export([
    start_link/1,
    start_link/2
    ]).

% Api functions
-export([
    create/2,
    lookup/1,
    validate/2,
    link_email/3,
    delete/3,
    undelete/2
    ]).
    
% Gen server callbacks
-export([
    init/1,
    stop/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    code_change/3,
    terminate/2
    ]).

% Server state
-record(state, {
    module, 
    mod_state
    }).

-include("account.hrl").

-define(KEY_BYTE_SIZE, 16).
-define(IV_BYTE_SIZE, 16).
-define(KEY_BIT_SIZE, 4096).

start_link(Module) ->
    start_link(?MODULE, Module).

start_link(ServerName, Module) ->
    case gen_server:start_link({local, ServerName}, ?MODULE, 
    [Module], []) of
        {ok, Pid} ->
            {ok, Pid};
        {error, {already_started, OldPid}} ->
            {ok, OldPid};
        Error ->
            error_logger:error_report([{"start_link/2:", Error}, "Error Here"])
    end.

create({email, Email}, Password) ->
    gen_server:call(?MODULE, {create, {email, Email}, {password, Password}});

create({name, Name}, Password) ->
    gen_server:call(?MODULE, {create, {name, Name}, {password, Password}});

create(_BadFormat, _Password) ->
    {error, bad_format}.

lookup({email, Email}) ->
    gen_server:call(?MODULE, {lookup_email, Email});

lookup({name, Name}) ->
    gen_server:call(?MODULE, {lookup_name, Name}).

validate({email, Email}, Password) ->
    gen_server:call(?MODULE, {validate, {email, Email}, {password, Password}});

validate({name, Name}, Password) ->
    gen_server:call(?MODULE, {validate, {name, Name}, {password, Password}}).

link_email(Name, Email, Password) ->
    gen_server:call(?MODULE, {link_email, {name, Name}, {email, Email}, {password, Password}}).

delete({name, Name}, Password, SoftOrHard) ->
    gen_server:call(?MODULE, {delete, {name, Name}, {password, Password}, SoftOrHard});

delete({email, Email}, Password, SoftOrHard) ->
    gen_server:call(?MODULE, {delete, {email, Email}, {password, Password}, SoftOrHard}).

undelete({name, Name}, Password) ->
    gen_server:call(?MODULE, {undelete, {name, Name}, {password, Password}});

undelete({email, Email}, Password) ->
    gen_server:call(?MODULE, {undelete, {email, Email}, {password, Password}}).

init([Module]) ->
    crypto:start(),
    bcrypt:start(),
    {ok, ModState} = Module:init(),
    {ok, #state{module = Module, mod_state = ModState}}.

stop(Server) ->
    gen_server:call(Server, stop).

handle_info(Info, State) ->
    error_logger:info_report([{"Info:", Info}]),
    {nopreply, State}.

handle_cast(Cast, State) ->
    error_logger:info_report([{"Cast:", Cast}]),
    {nopreply, State}.

handle_call({create, {email, Email}, {password, Password}}, _From, State) ->
    Module = State#state.module,
    ModState = State#state.mod_state,

    {ok, EmailHash} = account_util:hash_email(Email),

    case Module:exists({email, EmailHash}, ModState) of
        {ok, _Uid} ->
            {reply, {error, email_taken}, State};
        not_found ->
            {ok, PasswordHash} = account_util:hash_password(Password),
            {ok, EncryptedEmail} = account_util:encrypt(Email),
            {ok, _Result} = Module:create(
                             EmailHash,
                             PasswordHash,
                             EncryptedEmail,
                             <<"{}">>,
                             ModState),
            {reply, {ok, created}, State}
    end;

handle_call({create, {name, Name}, {password, Password}}, _From, State) ->
    Module = State#state.module,
    ModState = State#state.mod_state,

    {ok, NameHash} = account_util:hash_name(Name),

    case Module:exists({name, NameHash}, ModState) of
        {ok, _Uid} ->
            {reply, {error, name_taken}, State};
        not_found ->
            {ok, PasswordHash} = account_util:hash_password(Password),
            {ok, _Result} = Module:create(
                             NameHash,
                             PasswordHash,
                             <<"{}">>,
                             ModState),
            {reply, {ok, created}, State}
    end;


handle_call({lookup_email, Email}, _From, State) ->
    Module = State#state.module,
    ModState = State#state.mod_state,

    {ok, EmailHash} = account_util:hash_email(Email),

    case Module:exists({email, EmailHash}, ModState) of
        {ok, Uid} ->
            {reply, {ok, Uid}, State};
        not_found ->
            {reply, not_found, State}
    end;

handle_call({lookup_name, Name}, _From, State) ->
    Module = State#state.module,
    ModState = State#state.mod_state,

    {ok, NameHash} = account_util:hash_name(Name),

    case Module:exists({name, NameHash}, ModState) of
        {ok, Uid, Deleted} ->
            {reply, {ok, Uid, Deleted}, State};
        not_found ->
            {reply, not_found, State}
    end;

handle_call({validate, {email, Email}, {password, Password}}, _From, State) ->
    Module = State#state.module,
    ModState = State#state.mod_state,

    {ok, EmailHash} = account_util:hash_email(Email),

    case validate_password({email, EmailHash}, Password, Module, ModState) of
        true ->
            {reply, {ok, validated}, State};
        false ->
            {reply, {error, wrong_account_or_password}, State}
    end;

handle_call({validate, {name, Name}, {password, Password}}, _From, State) ->
    Module = State#state.module,
    ModState = State#state.mod_state,

    {ok, NameHash} = account_util:hash_name(Name),

    case validate_password({name, NameHash}, Password, Module, ModState) of
        true ->
            {reply, {ok, validated}, State};
        false ->
            {reply, {error, wrong_account_or_password}, State}
    end;

handle_call({link_email, {name, Name}, {email, Email}, {password, Password}}, _From, State) ->
    Module = State#state.module,
    ModState = State#state.mod_state,
    
    {ok, EmailHash} = account_util:hash_email(Email),
    {ok, NameHash} = account_util:hash_name(Name),
    
    case {Module:exists({email, EmailHash}, ModState), Module:get_password({name, NameHash}, ModState)} of 
        {{ok, _Uid}, _} ->
            case Module:get_email(NameHash, ModState) of
                {ok, EmailHash} ->
                    {reply, {error, email_already_linked}, State};
                E ->
                    error_logger:info_report({?MODULE, <<"E">>, E}),
                    {reply, {error, email_taken}, State}
            end;
        {not_found, not_found} ->
            {reply, {error, account_not_found}, State};
        {not_found, {ok, PasswordHash}} ->
            case account_util:validate_password(Password, PasswordHash) of
                true ->
                    {ok, EncryptedEmail} = account_util:encrypt(Email),
                    case Module:link_email(NameHash, EmailHash, EncryptedEmail, ModState) of
                        {ok, email_linked} ->
                            {reply, {ok, email_linked}, State};
                        Reason ->
                            error_logger:error_report({?MODULE, link_email, {error, Reason}}),
                            {error, Reason, State}
                    end;
                false ->
                    {reply, {ok, wrong_account_or_password}, State}
            end
    end;

handle_call({delete, {name, Name}, {password, Password}, HardOrSoft}, _From, State) ->
    Module = State#state.module,
    ModState = State#state.mod_state,

    {ok, NameHash} = account_util:hash_name(Name),
    case Module:exists({name, NameHash}, ModState) of
        not_found ->
            {reply, {error, wrong_account_or_password}, State};
        {ok, Uid} ->
            Result = delete_account(Uid, Password, HardOrSoft, Module, ModState),
            {reply, Result, State}
    end;

handle_call({delete, {email, Email}, {password, Password}, HardOrSoft}, _From, State) ->
    Module = State#state.module,
    ModState = State#state.mod_state,

    {ok, EmailHash} = account_util:hash_email(Email),
    case Module:exists({email, EmailHash}, ModState) of
        not_found ->
            {reply, {error, wrong_account_email_or_password}, State};
        {ok, Uid} ->
            Result = delete_account(Uid, Password, HardOrSoft, Module, ModState),
            {reply, Result, State}
    end;

handle_call({undelete, {name, Name}, {password, Password}}, _From, State) ->
    Module = State#state.module,
    ModState = State#state.mod_state,

    {ok, NameHash} = account_util:hash_name(Name),
    Result = undelete_account({name, NameHash}, Password, Module, ModState),
    {reply, Result, State};

handle_call({undelete, {email, Email}, {password, Password}}, _From, State) ->
    Module = State#state.module,
    ModState = State#state.mod_state,

    {ok, EmailHash} = account_util:hash_email(Email),
    Result = undelete_account({email, EmailHash}, Password, Module, ModState),
    {reply, Result, State};

handle_call(stop, _From, State) ->
    {stop, requested, State}.

code_change(OldVsn, State, Extra) ->
    error_logger:info_report([{"code_change:", OldVsn, Extra}]),
    {ok, State}.

terminate(Reason, State) ->
    Module = State#state.module,
    ModState = State#state.mod_state,
    Module:stop(ModState),
    error_logger:info_report([{"terminate:", Reason}]).

validate_password(EmailOrAccountName, Password, Module, ModState) ->
    case Module:get_password(EmailOrAccountName, ModState) of
        {ok, PasswordHash} ->
            account_util:validate_password(Password, PasswordHash);
        not_found ->
            false
    end.

delete_account(Uid, Password, HardOrSoft, Module, ModState) ->
    case validate_password({uid, Uid}, Password, Module, ModState) of
        true ->
            case HardOrSoft of
                hard ->
                    {ok, _} = Module:delete({uid, Uid}, ModState),
                    {ok, deleted};
                soft ->
                    {ok, _} = Module:set_deleted({uid, Uid}, true, ModState),
                    {ok, deleted}
            end;
        false ->
            {error, wrong_account_or_password}
    end.

undelete_account(NameOrEmailTuple, Password, Module, ModState) ->
    case Module:exists(NameOrEmailTuple, ModState) of
        not_found ->
            {error, wrong_account_or_password};
        {ok, Uid} ->
            validate_and_undelete_account(Uid, Password, not_deleted, Module, ModState);
        {ok, Uid, deleted} ->
            validate_and_undelete_account(Uid, Password, deleted, Module, ModState)
    end.

validate_and_undelete_account(Uid, Password, Deleted, Module, ModState) ->
    case {validate_password({uid, Uid}, Password, Module, ModState), Deleted} of
        {false, _} ->
            {error, wrong_account_or_password};
        {true, not_deleted} ->
            {error, not_deleted};
        {true, deleted} ->
            Module:set_deleted({uid, Uid}, false, ModState),
            {ok, undeleted}
    end.