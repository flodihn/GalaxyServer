-module(account_util).

-include("account.hrl").

-export([
    get_timestamp/0,
    hash_email/1,
    hash_password/1,
    hash_name/1,
    encrypt/1,
    validate_password/2
    ]).

get_timestamp() ->
    {MegaSecs, Secs, MicroSecs} = now(),
    (MegaSecs*1000000 + Secs)*1000000 + MicroSecs.

hash_name(Name) ->
    hash_email(Name).

hash_email(Email) ->
    {ok, hexify(crypto:hash(sha512, Email))}.

hash_password(Password) ->
    {ok, Salt} = bcrypt:gen_salt(),
    {ok, Hash} = bcrypt:hashpw(Password, Salt),
    {ok, list_to_binary(Hash)}.

hexify(Bin) when is_binary(Bin) ->
    << <<(hex(H)),(hex(L))>> || <<H:4,L:4>> <= Bin >>.

hex(C) when C < 10 -> $0 + C;
hex(C) -> $a + C - 10.

encrypt(Text) ->
    {ok, EncryptionKeyBase64} = application:get_env(account_srv, encryption_key),
    EncryptionKey = base64:decode(EncryptionKeyBase64),
    IV = crypto:strong_rand_bytes(16),
    Data = crypto:block_encrypt(aes_cfb128, EncryptionKey, IV, <<IV/binary, Text/binary>>),
    Base64Data = base64:encode(Data),
    {ok, Base64Data}.

validate_password(PasswordFromUser, PasswordHashFromDb) ->
    {ok, PasswordHashStr} = bcrypt:hashpw(PasswordFromUser, PasswordHashFromDb),
    %% maybe rework bcrypt to return binaries.
    PasswordHashAsBinary = list_to_binary(PasswordHashStr),
    PasswordHashAsBinary =:= PasswordHashFromDb.