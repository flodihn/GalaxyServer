-module(account_util).

-include("account.hrl").

-export([
    get_timestamp/0,
    hash_email/1,
    hash_password/1,
    hash_name/1,
    encrypt/1,
    validate_password/2,
    generate_uid/0
    ]).

get_timestamp() ->
    {MegaSecs, Secs, MicroSecs} = os:timestamp(),
    (MegaSecs*1000000 + Secs)*1000000 + MicroSecs.

hash_name(Name) ->
    hash_email(Name).

hash_email(Email) ->
    {ok, hexify(crypto:hash(sha512, Email))}.

hash_password(Password) ->
    Salt = crypto:strong_rand_bytes(16),
    Hash = crypto:hash(sha512, <<Salt/binary, (iolist_to_binary(Password))/binary>>),
    Encoded = base64:encode(<<Salt/binary, Hash/binary>>),
    {ok, Encoded}.

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
    Decoded = base64:decode(PasswordHashFromDb),
    <<Salt:16/binary, StoredHash/binary>> = Decoded,
    Computed = crypto:hash(sha512, <<Salt/binary, (iolist_to_binary(PasswordFromUser))/binary>>),
    Computed =:= StoredHash.

generate_uid() ->
    <<A:32, B:16, C:16, D:16, E:48>> = crypto:strong_rand_bytes(16),
    iolist_to_binary(io_lib:format("~8.16.0b-~4.16.0b-4~3.16.0b-~4.16.0b-~12.16.0b", [A, B, C band 16#0fff, (D band 16#3fff) bor 16#8000, E])).
