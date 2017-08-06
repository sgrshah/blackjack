-module(simulation).
-behavior(gen_server).

% External API
-export([start_link/0, trigger/2, process_outcome/2, poll/1]).
% Gen_server callbacks API
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

% Public Facing API:

% `{ok, Pid} = simulation:start_link().`
% We want to use start_link here because if the child process (simulation) dies, then we want to the
% parent process to die as well.
start_link() -> gen_server:start_link(?MODULE, [], []).

% `simulation:trigger(Pid, stand).`
% Async cast to specified Pid.
% Returns ok.
trigger(Pid, TestType) ->
  gen_server:cast(Pid, {trigger, TestType}).

% Async cast with incoming outcome from a game.
% This external API function is called by the game when it is finished.
process_outcome(Pid, GameOutcome) ->
  gen_server:cast(Pid, {update_state, GameOutcome}).

% Synchronous call to check for the state of the simulation.
% Returns the state of the simulation, i.e. the expected value of each upcard/playerhand.
poll(Pid) ->
  gen_server:call(Pid, {poll}).

% GenServer callbacks:

% Used to initialize the server's state. In this case we initialize it with an empty orddict to
% populate with our results.
init([]) ->
  {ok, SupPid} = game_supervisor:start_link(),
  State = {generate_results_list(), SupPid},
  {ok, State}.

% Handle cast will handle requests that are meant to be async. For example, triggering simulation
% (called by the user client) and updating the state (called by a finished game).
handle_cast({trigger, TestType}, {ResultsList, SupPid}) ->
  % Trigger simulation based on TestType specified.
  trigger_simulations(TestType, orddict:to_list(ResultsList), SupPid),
  {noreply, {ResultsList, SupPid}};
handle_cast({update_state, GameOutcome}, {ResultsList, SupPid}) ->
  % Update state based on incoming game outcome.
  UpdatedState = update_results_list(ResultsList, GameOutcome),
  {noreply, {UpdatedState, SupPid}}.

% Handle call will handle requests that are meant to be synchronous. For example, polling for status
% (called by the user client).
handle_call({poll}, From, {ResultsList, SupPid}) ->
  {reply, ResultsList, {ResultsList, SupPid}}.

% Handle info will manage messages that the cast/call paradigm does not know how to answer.
handle_info(Info, State) ->
  io:format("Don't know what to do with ~p~n", [Info]),
  {noreply, State}.

% I have no idea what terminate does either.
terminate(Reason, State) ->
  io:format("shutting down."),
  ok.

% I have no idea how code_change works.
code_change(OldVsn, State, Extra) -> {ok, State}.

% Private functions
trigger_simulations(TestType, [], SupPid) ->
  ok;

trigger_simulations(TestType, [{{UpCard, PlayerHandCount}, _}|T], SupPid) ->
  trigger_simulations(TestType, {UpCard, PlayerHandCount}, SupPid),
  trigger_simulations(TestType, T, SupPid);

trigger_simulations(TestType, HandCombination, SupPid) ->
  trigger_simulations(TestType, HandCombination, 0, SupPid).

trigger_simulations(_, _, SimulationCount, SupPid) when SimulationCount >= 500 ->
  ok;

trigger_simulations(TestType, HandCombination, SimulationCount, SupPid) when SimulationCount < 500 ->
  supervisor:start_child(SupPid, [TestType, HandCombination, self()]),
  %% game_server:trigger(Pid, self()),
  trigger_simulations(TestType, HandCombination, SimulationCount + 1, SupPid).

generate_results_list() ->
  generate_results_row([], 17).

generate_results_row(ResultsList, PlayerHandCount) when PlayerHandCount < 8 ->
  orddict:from_list(ResultsList);
generate_results_row(ResultsList, PlayerHandCount) when (PlayerHandCount =< 17) and (PlayerHandCount
                                                                                     >= 8) ->
  ResultOrddict = orddict:from_list([{simulations_count, 0}, {expected_value, 0}]),

  Row = [
   {{2, PlayerHandCount}, ResultOrddict},
   {{3, PlayerHandCount}, ResultOrddict},
   {{4, PlayerHandCount}, ResultOrddict},
   {{5, PlayerHandCount}, ResultOrddict},
   {{6, PlayerHandCount}, ResultOrddict},
   {{7, PlayerHandCount}, ResultOrddict},
   {{8, PlayerHandCount}, ResultOrddict},
   {{9, PlayerHandCount}, ResultOrddict},
   {{10, PlayerHandCount}, ResultOrddict},
   {{{1,11}, PlayerHandCount}, ResultOrddict}
  ],
  generate_results_row(ResultsList ++ Row, PlayerHandCount - 1).

update_results_list(SimulationResults, GameResults) ->
  %% io:format("Simulation #:~w~n", [GameResults]),
  {player, PlayerHand, dealer, DealerHand} = lists:nth(1, GameResults),
  PlayerHandCount = lists:nth(1, PlayerHand),
  if
    (PlayerHandCount =< 17) and (PlayerHandCount >= 8) ->
      {expected_value, IncomingExpectedValue} = lists:nth(2, GameResults),
      UpCard = lists:nth(2, DealerHand),
      UpdateExpectedValueFun = fun(SimulationOrddict) ->
          ExistingSimulationCount = orddict:fetch(simulations_count, SimulationOrddict),
          ExistingExpectedValue = orddict:fetch(expected_value, SimulationOrddict),
          TotalValue = ExistingSimulationCount * ExistingExpectedValue,
          NewSimulationCount = ExistingSimulationCount + 1,
          NewTotalValue = TotalValue + IncomingExpectedValue,
          NewExpectedValue = NewTotalValue / NewSimulationCount,
          orddict:from_list([{simulations_count, NewSimulationCount}, {expected_value, NewExpectedValue}])
      end,
      orddict:update({UpCard, PlayerHandCount}, UpdateExpectedValueFun, SimulationResults);
    (PlayerHandCount > 17) or (PlayerHandCount < 8) ->
      SimulationResults
  end.

