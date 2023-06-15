-module(game_mochiglobal).

%% API
-export([
    create/2
]).

%%====================================================================
%% API functions
%%====================================================================

create(ModName, PropLists) ->
    ok = fling_mochiglobal:create(ModName, PropLists, fun get_key/1, fun get_value/1).

%%====================================================================
%% Internal functions
%%====================================================================

get_key({K, _V}) -> K.
get_value({_K, V}) -> V.
