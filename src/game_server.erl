-module(game_server).
-behavior(gen_server).

% External API
-export([start_link/3, trigger/2]).
% Gen_server callbacks API
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-define(CHAOS_MONKEY, false).

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
start_link(TestType, {UpCard, PlayerHandCount}, From) ->
  {ok, Pid} = gen_server:start_link(?MODULE, [TestType, {UpCard, PlayerHandCount}], []),
  trigger(Pid, From),
  {ok, Pid}.

% `game:play(Pid, TestType).`
% Async cast to specified Pid.
% Returns ok.
trigger(Pid, From) ->
  gen_server:cast(Pid, {play, From}).

% GenServer Callbacks:
init([TestType, {UpCard, PlayerHandCount}]) ->
  {ok, {TestType, {UpCard, PlayerHandCount}}}.

play(Pid) ->
  {GameState, Hands, ExpectedValue} = game:trigger(Pid),

  case GameState of
    player_win -> [Hands, {expected_value, ExpectedValue}];
    dealer_win -> [Hands, {expected_value, ExpectedValue}];
    push -> [Hands, {expected_value, ExpectedValue}];
    _ -> play(Pid)
  end.

handle_cast({play, From}, {TestType, {UpCard, PlayerHandCount}}) ->
  {ok, Pid} = game:start_link(TestType, {UpCard, PlayerHandCount}),
  Result = play(Pid),

  if
    ?CHAOS_MONKEY =:= true -> chaos_monkey(From, Result);
    ?CHAOS_MONKEY =/= true -> spin_down_game_server(From, Result)
  end.

spin_down_game_server(From, Result) ->
  simulation:process_outcome(From, Result),
  {stop, normal, Result}.

chaos_monkey(From, Result) ->
  RandNum = rand:uniform(),
  if
    RandNum > 0.90 -> erlang:error(chaos);
    RandNum =< 0.90 -> spin_down_game_server(From, Result)
  end.

handle_call(Request, From, State) ->
  {reply, State}.

handle_info(Info, State) ->
  io:format("Don't know what to do with ~p~n", [Info]),
  {noreply, State}.

terminate(Reason, State) ->
  io:format("shutting down."),
  ok.

code_change(OldVsn, State, Extra) -> {ok, State}.
