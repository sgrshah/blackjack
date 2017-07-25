-module(game).
-behavior(gen_server).

% External API
-export([start/0, trigger/3, play/3]).
% Gen_server callbacks API
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

% This module encapsulates all high level game logic
% Hardcoded rules:
% Dealer stands on soft 17.
% 1 deck
% No splits

% External API:
% `{ok, Pid} = game:start().
% We want to use start here because if the child (game) dies we don't want the parent (simulation) to die
% as well.
start() -> gen_server:start(?MODULE, [], []).

% `game:play(Pid, TestType).`
% Async cast to specified Pid.
% Returns ok.
trigger(Pid, TestType, From) ->
  gen_server:cast(Pid, {play, TestType, From}).

% GenServer Callbacks:
init([]) ->
  State = [],
  {ok, State}.

handle_cast({play, TestType, From}, State) ->
  Shoe = shoe:create({decks, 1}),
  InitialHands = dealer:deal(Shoe),
  Result = play(Shoe, player, InitialHands, TestType),
  Output = [Result, determine_expected_value(Result)],
  simulation:process_outcome(From, Output),
  {noreply, Output}.

handle_call(Request, From, State) ->
  {reply, State}.

handle_info(Info, State) ->
  io:format("Don't know what to do with ~p~n", [Info]),
  {noreply, State}.

terminate(Reason, State) ->
  io:format("shutting down."),
  ok.

code_change(OldVsn, State, Extra) -> {ok, State}.

play(Shoe, player, {player, PlayerHand, dealer, DealerHand}, TestType) ->
  PlayerCount = hand:sum(PlayerHand),

  if
    PlayerCount == 21 ->
      {player, PlayerHand, dealer, DealerHand};
    PlayerCount < 21 ->
      actions:decide(Shoe, player, {player, PlayerHand, dealer, DealerHand}, TestType)
  end.

play(Shoe, dealer, {player, PlayerHand, dealer, DealerHand}) ->

  DealerCount = hand:sum(DealerHand),

  if
    DealerCount == 21 ->
      {player, PlayerHand, dealer, DealerHand};
    DealerCount < 21 ->
      actions:decide(Shoe, dealer, {player, PlayerHand, dealer, DealerHand})
  end.

determine_expected_value({player, PlayerHand, dealer, DealerHand}) ->
  PlayerCount = hand:sum(PlayerHand),
  DealerCount = hand:sum(DealerHand),

  if
    PlayerCount > 21 -> {expected_value, -1};
    DealerCount > 21 -> {expected_value, 1};
    PlayerCount > DealerCount -> {expected_value, 1};
    PlayerCount ==  DealerCount -> {expected_value, 0};
    PlayerCount <  DealerCount -> {expected_value, -1}
  end.
