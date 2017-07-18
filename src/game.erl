-module(game).
-export([play/0, play/3]).

% This module encapsulates all high level game logic

play() ->
  Shoe = shoe:create({decks, 1}),
  io:format("~p~n", [Shoe]),
  InitialHands = dealer:deal(Shoe),
  Result = play(Shoe, player, InitialHands),
  io:format("Result~p~n", [Result]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

play(Shoe, player, {player, PlayerHand, dealer, DealerHand}) ->
  PlayerCount = hand:sum(PlayerHand),
  io:format("PlayerCount~p~n", [PlayerCount]),

  if
    PlayerCount == 21 ->
      io:format("player wins blackjack~n"),
      {player, PlayerHand, dealer, DealerHand};
    PlayerCount < 21 ->
      io:format("must complete game~n"),
      actions:decide(Shoe, player, {player, PlayerHand, dealer, DealerHand})
  end;

play(Shoe, dealer, {player, PlayerHand, dealer, DealerHand}) ->
  io:format("trying to complete dealer's hand~n"),

  DealerCount = hand:sum(DealerHand),
  io:format("DealerCount: ~p~n", [DealerCount]),

  if
    DealerCount == 21 ->
      io:format("dealer has blackjack~n"),
      {player, DealerHand, dealer, DealerHand};
    DealerCount < 21 ->
      io:format("dealer must make decision~n"),
      actions:decide(Shoe, dealer, {player, PlayerHand, dealer, DealerHand})
  end.

