-module(galaxy_util).

%% Application callbacks
-export([
    sector_name_to_sup/1
    ]).

sector_name_to_sup(SectorName) ->
    list_to_atom(atom_to_list(SectorName) ++ "_sup").

