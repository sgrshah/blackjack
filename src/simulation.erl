-module(simulation).
-export([simulation/2]).

simulation(ParentProcess, SimulationResults) ->
  receive
    {_, {_, GameResult}} when is_list(GameResult) ->
      NewSimulationResults = update_results_list(SimulationResults, GameResult),
      ParentProcess ! {self(), NewSimulationResults},
      simulation(ParentProcess, NewSimulationResults);
    {From, Message} when is_atom(Message) ->
      io:format("starting a batch simulation..."),
      trigger_simulations(Message),
      NewSimulationResults = generate_results_list(),
      simulation(From, NewSimulationResults)
  end.

trigger_simulations(Directive) ->
  trigger_simulations(Directive, 0).

trigger_simulations(_, SimulationCount) when SimulationCount >= 10000 ->
  ok;

trigger_simulations(Directive, SimulationCount) when SimulationCount < 10000 ->
  Game = spawn(game, play, []),
  Game ! {self(), Directive},
  trigger_simulations(Directive, SimulationCount + 1).

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
  PlayerHandCount = hand:sum([lists:nth(1, PlayerHand), lists:nth(2, PlayerHand)]),
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

