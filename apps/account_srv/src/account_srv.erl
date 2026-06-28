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
    create/3,
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

create({username, Username}, Password) ->
    gen_server:call(?MODULE, {create, {username, Username}, {password, Password}});

create(_BadFormat, _Password) ->
    {error, bad_format}.

create({username, Username}, {email, Email}, Password) ->
    gen_server:call(?MODULE, {create, {username, Username}, {email, Email}, {password, Password}});

create(_BadFormat, _Email, _Password) ->
    {error, bad_format}.

lookup({username, Username}) ->
    gen_server:call(?MODULE, {lookup_username, Username});

lookup({email, Email}) ->
    gen_server:call(?MODULE, {lookup_email, Email}).

validate({username, Username}, Password) ->
    gen_server:call(?MODULE, {validate, {username, Username}, {password, Password}});

validate({email, Email}, Password) ->
    gen_server:call(?MODULE, {validate, {email, Email}, {password, Password}}).

link_email(Username, Email, Password) ->
    gen_server:call(?MODULE, {link_email, {username, Username}, {email, Email}, {password, Password}}).

delete({username, Username}, Password, SoftOrHard) ->
    gen_server:call(?MODULE, {delete, {username, Username}, {password, Password}, SoftOrHard});

delete({email, Email}, Password, SoftOrHard) ->
    gen_server:call(?MODULE, {delete, {email, Email}, {password, Password}, SoftOrHard}).

undelete({username, Username}, Password) ->
    gen_server:call(?MODULE, {undelete, {username, Username}, {password, Password}});

undelete({email, Email}, Password) ->
    gen_server:call(?MODULE, {undelete, {email, Email}, {password, Password}}).

init([Module]) ->
    crypto:start(),
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

handle_call({create, {username, Username}, {email, Email}, {password, Password}}, _From, State) ->
    Module = State#state.module,
    ModState = State#state.mod_state,
    case account_util:normalize_username(Username) of
        {error, invalid_username} ->
            {reply, {error, invalid_username}, State};
        {ok, UsernameBin} ->
            case account_util:normalize_optional_email(Email) of
                undefined ->
                    {reply, create_username_only(UsernameBin, Password, Module, ModState), State};
                {ok, EmailBin} ->
                    {reply, create_username_with_email(UsernameBin, EmailBin, Password, Module, ModState), State}
            end
    end;

handle_call({create, {username, Username}, {password, Password}}, _From, State) ->
    Module = State#state.module,
    ModState = State#state.mod_state,
    case account_util:normalize_username(Username) of
        {error, invalid_username} ->
            {reply, {error, invalid_username}, State};
        {ok, UsernameBin} ->
            {reply, create_username_only(UsernameBin, Password, Module, ModState), State}
    end;

handle_call({lookup_username, Username}, _From, State) ->
    Module = State#state.module,
    ModState = State#state.mod_state,
    case account_util:normalize_username(Username) of
        {error, invalid_username} ->
            {reply, not_found, State};
        {ok, UsernameBin} ->
            case Module:exists({username, UsernameBin}, ModState) of
                {ok, FoundUsername, deleted} ->
                    {reply, {ok, FoundUsername, deleted}, State};
                {ok, FoundUsername} ->
                    {reply, {ok, FoundUsername}, State};
                not_found ->
                    {reply, not_found, State}
            end
    end;

handle_call({lookup_email, Email}, _From, State) ->
    Module = State#state.module,
    ModState = State#state.mod_state,
    {ok, EmailHash} = account_util:hash_email(Email),
    case Module:exists({email, EmailHash}, ModState) of
        {ok, Username, deleted} ->
            {reply, {ok, Username, deleted}, State};
        {ok, Username} ->
            {reply, {ok, Username}, State};
        not_found ->
            {reply, not_found, State}
    end;

handle_call({validate, {username, Username}, {password, Password}}, _From, State) ->
    Module = State#state.module,
    ModState = State#state.mod_state,
    case account_util:normalize_username(Username) of
        {error, invalid_username} ->
            {reply, {error, wrong_account_or_password}, State};
        {ok, UsernameBin} ->
            case validate_password({username, UsernameBin}, Password, Module, ModState) of
                true ->
                    {reply, {ok, validated}, State};
                false ->
                    {reply, {error, wrong_account_or_password}, State}
            end
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

handle_call({link_email, {username, Username}, {email, Email}, {password, Password}}, _From, State) ->
    Module = State#state.module,
    ModState = State#state.mod_state,
    case account_util:normalize_username(Username) of
        {error, invalid_username} ->
            {reply, {error, account_not_found}, State};
        {ok, UsernameBin} ->
            {ok, EmailHash} = account_util:hash_email(Email),
            case {Module:exists({email, EmailHash}, ModState),
                  Module:get_password({username, UsernameBin}, ModState)} of
                {{ok, _}, _} ->
                    case Module:get_email(UsernameBin, ModState) of
                        {ok, EmailHash} ->
                            {reply, {error, email_already_linked}, State};
                        _ ->
                            {reply, {error, email_taken}, State}
                    end;
                {not_found, not_found} ->
                    {reply, {error, account_not_found}, State};
                {not_found, {ok, PasswordHash}} ->
                    case account_util:validate_password(Password, PasswordHash) of
                        true ->
                            {ok, EncryptedEmail} = account_util:encrypt(Email),
                            case Module:link_email(UsernameBin, EmailHash, EncryptedEmail, ModState) of
                                {ok, email_linked} ->
                                    {reply, {ok, email_linked}, State};
                                Reason ->
                                    error_logger:error_report({?MODULE, link_email, {error, Reason}}),
                                    {reply, {error, Reason}, State}
                            end;
                        false ->
                            {reply, {error, wrong_account_or_password}, State}
                    end
            end
    end;

handle_call({delete, {username, Username}, {password, Password}, HardOrSoft}, _From, State) ->
    Module = State#state.module,
    ModState = State#state.mod_state,
    case account_util:normalize_username(Username) of
        {error, invalid_username} ->
            {reply, {error, wrong_account_or_password}, State};
        {ok, UsernameBin} ->
            case Module:exists({username, UsernameBin}, ModState) of
                not_found ->
                    {reply, {error, wrong_account_or_password}, State};
                {ok, _} ->
                    Result = delete_account(UsernameBin, Password, HardOrSoft, Module, ModState),
                    {reply, Result, State}
            end
    end;

handle_call({delete, {email, Email}, {password, Password}, HardOrSoft}, _From, State) ->
    Module = State#state.module,
    ModState = State#state.mod_state,
    {ok, EmailHash} = account_util:hash_email(Email),
    case Module:exists({email, EmailHash}, ModState) of
        not_found ->
            {reply, {error, wrong_account_email_or_password}, State};
        {ok, Username} ->
            Result = delete_account(Username, Password, HardOrSoft, Module, ModState),
            {reply, Result, State}
    end;

handle_call({undelete, {username, Username}, {password, Password}}, _From, State) ->
    Module = State#state.module,
    ModState = State#state.mod_state,
    case account_util:normalize_username(Username) of
        {error, invalid_username} ->
            {reply, {error, wrong_account_or_password}, State};
        {ok, UsernameBin} ->
            Result = undelete_account({username, UsernameBin}, Password, Module, ModState),
            {reply, Result, State}
    end;

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

create_username_only(UsernameBin, Password, Module, ModState) ->
    case Module:exists({username, UsernameBin}, ModState) of
        {ok, _} ->
            {error, username_taken};
        {ok, _, _} ->
            {error, username_taken};
        not_found ->
            {ok, PasswordHash} = account_util:hash_password(Password),
            Module:create(UsernameBin, PasswordHash, <<"{}">>, ModState)
    end.

create_username_with_email(UsernameBin, EmailBin, Password, Module, ModState) ->
    {ok, EmailHash} = account_util:hash_email(EmailBin),
    case {Module:exists({username, UsernameBin}, ModState),
          Module:exists({email, EmailHash}, ModState)} of
        {{ok, _}, _} ->
            {error, username_taken};
        {_, {ok, _}} ->
            {error, email_taken};
        {_, {ok, _, _}} ->
            {error, email_taken};
        {not_found, not_found} ->
            {ok, PasswordHash} = account_util:hash_password(Password),
            {ok, EncryptedEmail} = account_util:encrypt(EmailBin),
            Module:create(
                UsernameBin,
                EmailHash,
                PasswordHash,
                EncryptedEmail,
                <<"{}">>,
                ModState)
    end.

validate_password(UsernameOrEmail, Password, Module, ModState) ->
    case Module:get_password(UsernameOrEmail, ModState) of
        {ok, PasswordHash} ->
            account_util:validate_password(Password, PasswordHash);
        not_found ->
            false
    end.

delete_account(Username, Password, HardOrSoft, Module, ModState) ->
    case validate_password({username, Username}, Password, Module, ModState) of
        true ->
            case HardOrSoft of
                hard ->
                    {ok, _} = Module:delete({username, Username}, ModState),
                    {ok, deleted};
                soft ->
                    {ok, _} = Module:set_deleted({username, Username}, true, ModState),
                    {ok, deleted}
            end;
        false ->
            {error, wrong_account_or_password}
    end.

undelete_account(UsernameOrEmailTuple, Password, Module, ModState) ->
    case Module:exists(UsernameOrEmailTuple, ModState) of
        not_found ->
            {error, wrong_account_or_password};
        {ok, Username} ->
            validate_and_undelete_account(Username, Password, not_deleted, Module, ModState);
        {ok, Username, deleted} ->
            validate_and_undelete_account(Username, Password, deleted, Module, ModState)
    end.

validate_and_undelete_account(Username, Password, Deleted, Module, ModState) ->
    case {validate_password({username, Username}, Password, Module, ModState), Deleted} of
        {false, _} ->
            {error, wrong_account_or_password};
        {true, not_deleted} ->
            {error, not_deleted};
        {true, deleted} ->
            Module:set_deleted({username, Username}, false, ModState),
            {ok, undeleted}
    end.