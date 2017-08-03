-module(game_server).
-behavior(gen_server).

% External API
-export([start_link/0, trigger/3]).
% Gen_server callbacks API
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

% This module encapsulates all high level game logic
% Hardcoded rules:
% Dealer stands on soft 17.
% 1 deck
% No splits

% External API:
% `{ok, Pid} = game:start_link().
% We were previously using start here because if the child (game_server) dies we don't want the
% parent (simulation) to die as well. However, now that we are implementing supervisors, the
% supervisor can use the link to restart the game_server.
start_link() ->
  gen_server:start_link(?MODULE, [], []).

% `game:play(Pid, TestType).`
% Async cast to specified Pid.
% Returns ok.
trigger(Pid, TestType, From) ->
  gen_server:cast(Pid, {play, TestType, From}).

% GenServer Callbacks:
init([]) ->
  State = [],
  {ok, State}.

play(Pid) ->
  {GameState, Hands, ExpectedValue} = game:trigger(Pid),

  case GameState of
    player_win -> [Hands, {expected_value, ExpectedValue}];
    dealer_win -> [Hands, {expected_value, ExpectedValue}];
    push -> [Hands, {expected_value, ExpectedValue}];
    _ -> play(Pid)
  end.

handle_cast({play, TestType, From}, State) ->
  {ok, Pid} = game:start_link(),
  Result = play(Pid),
  simulation:process_outcome(From, Result),
  {noreply, Result}.

handle_call(Request, From, State) ->
  {reply, State}.

handle_info(Info, State) ->
  io:format("Don't know what to do with ~p~n", [Info]),
  {noreply, State}.

terminate(Reason, State) ->
  io:format("shutting down."),
  ok.

code_change(OldVsn, State, Extra) -> {ok, State}.
