-module(game).
-export([play/1, play/3, play/4]).

% This module encapsulates all high level game logic
%
% Hardcoded rules:
% Dealer stands on soft 17.
% 1 deck
% No splits

play(TestType) ->
  Shoe = shoe:create({decks, 1}),
  io:format("~p~n", [Shoe]),
  InitialHands = dealer:deal(Shoe),
  Result = play(Shoe, player, InitialHands, TestType),
  Response = [Result, determine_expected_value(Result)],
  io:format("Result~w~n", [Response]).

play(Shoe, player, {player, PlayerHand, dealer, DealerHand}, TestType) ->
  io:format("player's turn~n"),
  PlayerCount = hand:sum(PlayerHand),

  if
    PlayerCount == 21 ->
      io:format("PlayerCount:~p~n", [PlayerCount]),
      io:format("player wins blackjack~n"),
      {player, PlayerHand, dealer, DealerHand};
    PlayerCount < 21 ->
      actions:decide(Shoe, player, {player, PlayerHand, dealer, DealerHand}, TestType)
  end.

play(Shoe, dealer, {player, PlayerHand, dealer, DealerHand}) ->
  io:format("dealer's turn~n"),

  DealerCount = hand:sum(DealerHand),

  if
    DealerCount == 21 ->
      io:format("DealerCount:~p~n", [DealerCount]),
      io:format("dealer has blackjack~n"),
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
