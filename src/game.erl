-module(game).
-export([play/0]).

% This module encapsulates all high level game logic
%
% Hardcoded rules:
% Dealer stands on soft 17.
% 1 deck
% No splits

play() ->
  receive
    {From, TestType} ->
      Result = play(TestType),
      From ! {self(), {TestType, Result}};
    _ ->
      io:format("dunno what to do here.")
  end.

play(TestType) ->
  Shoe = shoe:create({decks, 1}),
  InitialHands = dealer:deal(Shoe),
  Result = play(Shoe, player, InitialHands, TestType),
  [Result, determine_expected_value(Result)].


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
