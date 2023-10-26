%%%-------------------------------------------------------------------
%% @doc game_base top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(game_base_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    SupFlags = #{
        strategy => one_for_all,
        intensity => 0,
        period => 1
    },
    ChildSpecs = [
        #{
            id => game_uuid,
            start => {game_uuid, start_link, []},
            modules => [game_uuid],
            restart => permanent,
            shutdown => 5000,
            type => worker
        }
    ],
    {ok, {SupFlags, ChildSpecs}}.
