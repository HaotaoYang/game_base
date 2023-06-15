%%%-------------------------------------------------------------------
%% @doc game_base public API
%% @end
%%%-------------------------------------------------------------------

-module(game_base_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    game_base_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
