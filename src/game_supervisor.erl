-module(game_supervisor).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
  supervisor:start_link(?MODULE, []).

init([]) ->
  SupervisorFlags = #{
    strategy => simple_one_for_one,
    intensity => 50000,
    period => 600
  },
  ChildSpecs = [#{
    id => game_server,
    start => {game_server, start_link, []},
    restart => transient,
    %% shutdown => brutal_kill,
    type => worker,
    modules => [game_server]
  }],
  {ok, {SupervisorFlags, ChildSpecs}}.

