-module(account_mnesia).

-include("account.hrl").

-export([
    init/0,
    stop/1,
    create/5,
    create/4,
    delete/2,
    set_deleted/3,
    exists/2,
    get_password/2,
    get_email/2,
    link_email/4
]).

init() ->
    mnesia:start(),
    ensure_account_table(),
    {ok, []}.

ensure_account_table() ->
    case lists:member(account, mnesia:system_info(tables)) of
        true ->
            ok;
        false ->
            mnesia:change_config(extra_db_nodes, [node()]),
            mnesia:change_table_copy_type(schema, node(), disc_copies),
            Attributes = record_info(fields, account),
            mnesia:create_table(account, [
                {record_name, account},
                {disc_copies, [node()]},
                {attributes, Attributes}
            ]),
            ok
    end.

stop(_State) ->
    ok.

create(EmailHash, PasswordHash, EncryptedEmail, AccountJson, _State) ->
    Uid = account_util:generate_uid(),
    Now = account_util:get_timestamp(),
    Account = #account{
        uid = Uid,
        name_hash = undefined,
        email_hash = EmailHash,
        encrypted_email = EncryptedEmail,
        password_hash = PasswordHash,
        creation_time = Now,
        deleted = false,
        data = AccountJson,
        characters = []
    },
    T = fun() ->
        mnesia:write(account, Account, write)
    end,
    case mnesia:transaction(T) of
        {atomic, ok} ->
            {ok, created};
        {aborted, Reason} ->
            {error, Reason}
    end.

create(NameHash, PasswordHash, AccountJson, _State) ->
    Uid = account_util:generate_uid(),
    Now = account_util:get_timestamp(),
    Account = #account{
        uid = Uid,
        name_hash = NameHash,
        email_hash = undefined,
        encrypted_email = undefined,
        password_hash = PasswordHash,
        creation_time = Now,
        deleted = false,
        data = AccountJson,
        characters = []
    },
    T = fun() ->
        mnesia:write(account, Account, write)
    end,
    case mnesia:transaction(T) of
        {atomic, ok} ->
            {ok, created};
        {aborted, Reason} ->
            {error, Reason}
    end.

exists({email, EmailHash}, _State) ->
    T = fun() ->
        mnesia:match_object(account, #account{email_hash = EmailHash, _ = '_'}, read)
    end,
    case mnesia:transaction(T) of
        {atomic, []} ->
            not_found;
        {atomic, [#account{uid=Uid, deleted=true}]} ->
            {ok, Uid, deleted};
        {atomic, [#account{uid=Uid}]} ->
            {ok, Uid};
        {aborted, _Reason} ->
            not_found
    end;
exists({name, NameHash}, _State) ->
    T = fun() ->
        mnesia:match_object(account, #account{name_hash = NameHash, _ = '_'}, read)
    end,
    case mnesia:transaction(T) of
        {atomic, []} ->
            not_found;
        {atomic, [#account{uid=Uid, deleted=true}]} ->
            {ok, Uid, deleted};
        {atomic, [#account{uid=Uid}]} ->
            {ok, Uid};
        {aborted, _Reason} ->
            not_found
    end.

get_password({email, EmailHash}, _State) ->
    T = fun() ->
        mnesia:match_object(account, #account{email_hash = EmailHash, _ = '_'}, read)
    end,
    case mnesia:transaction(T) of
        {atomic, [#account{password_hash = Pass}]} ->
            {ok, Pass};
        {atomic, []} ->
            not_found;
        {aborted, _} ->
            not_found
    end;
get_password({name, NameHash}, _State) ->
    T = fun() ->
        mnesia:match_object(account, #account{name_hash = NameHash, _ = '_'}, read)
    end,
    case mnesia:transaction(T) of
        {atomic, [#account{password_hash = Pass}]} ->
            {ok, Pass};
        {atomic, []} ->
            not_found;
        {aborted, _} ->
            not_found
    end;
get_password({uid, Uid}, _State) ->
    T = fun() ->
        mnesia:read(account, Uid, read)
    end,
    case mnesia:transaction(T) of
        {atomic, [#account{password_hash = Pass}]} ->
            {ok, Pass};
        {atomic, []} ->
            not_found;
        {aborted, _} ->
            not_found
    end.

get_email(NameHash, _State) ->
    T = fun() ->
        mnesia:match_object(account, #account{name_hash = NameHash, _ = '_'}, read)
    end,
    case mnesia:transaction(T) of
        {atomic, [#account{email_hash = EmailHash}]} when EmailHash =/= undefined ->
            {ok, EmailHash};
        {atomic, []} ->
            not_found;
        {aborted, _} ->
            not_found
    end.

delete({uid, Uid}, _State) ->
    T = fun() ->
        mnesia:delete({account, Uid})
    end,
    case mnesia:transaction(T) of
        {atomic, ok} ->
            {ok, deleted};
        {aborted, Reason} ->
            {error, Reason}
    end.

set_deleted({uid, Uid}, TrueOrFalse, _State) ->
    T = fun() ->
        case mnesia:read(account, Uid, write) of
            [Acc] ->
                mnesia:write(account, Acc#account{deleted = TrueOrFalse}, write),
                ok;
            [] ->
                ok
        end
    end,
    case mnesia:transaction(T) of
        {atomic, ok} ->
            {ok, deleted_set};
        {aborted, Reason} ->
            {error, Reason}
    end.

link_email(NameHash, EmailHash, EncryptedEmail, _State) ->
    T = fun() ->
        case mnesia:match_object(account, #account{name_hash = NameHash, _ = '_'}, write) of
            [Acc] ->
                Updated = Acc#account{
                    email_hash = EmailHash,
                    encrypted_email = EncryptedEmail
                },
                mnesia:write(account, Updated, write),
                {ok, email_linked};
            [] ->
                {error, email_not_linked}
        end
    end,
    case mnesia:transaction(T) of
        {atomic, {ok, email_linked}} ->
            {ok, email_linked};
        {atomic, {error, Reason}} ->
            {error, Reason};
        {aborted, Reason} ->
            {error, Reason}
    end.

