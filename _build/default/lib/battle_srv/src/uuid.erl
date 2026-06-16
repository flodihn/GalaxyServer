-module(uuid).

-export([generate/0]).

%% Simple UUID v4 generator in binary string format.
%% Replaces the broken external dependency.
generate() ->
    <<A:32, B:16, C:16, D:16, E:48>> = crypto:strong_rand_bytes(16),
    iolist_to_binary(io_lib:format("~8.16.0b-~4.16.0b-4~3.16.0b-~4.16.0b-~12.16.0b",
                                   [A, B, C band 16#0fff, (D band 16#3fff) bor 16#8000, E])).
