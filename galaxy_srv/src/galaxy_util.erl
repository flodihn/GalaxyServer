-module(galaxy_util).

-include("galaxy_defs.hrl").
-include("resource_defs.hrl").

-ifdef(TEST).
-compile(export_all).
-endif.

%% Application callbacks
-export([
    sector_name_to_sup/1,
	new_structure/1,
	generate_uid/0
    ]).

sector_name_to_sup(SectorName) ->
    list_to_atom(atom_to_list(SectorName) ++ "_sup").

new_structure(StructureType) ->
	StructureUid = generate_uid(),
	#structure{uid=StructureUid, name=StructureType}.

generate_uid() ->
    <<A:32, B:16, C:16, D:16, E:48>> = crypto:rand_bytes(16),
    Str = io_lib:format("~8.16.0b-~4.16.0b-4~3.16.0b-~4.16.0b-~12.16.0b", 
                        [A, B, C band 16#0fff, D band 16#3fff bor 16#8000, E]),
    list_to_binary(Str).