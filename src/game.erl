-module(game).
-export([play/0, complete_hand/3]).

% This module encapsulates all high level game logic

play() ->
  Shoe = shoe:create({decks, 1}),
  io:format("~p~n", [Shoe]),
  Result = deal(Shoe),
  io:format("Result~p~n", [Result]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

deal(Shoe) ->
  deal(Shoe, []).

deal(Shoe, []) ->
  deal(Shoe, {player, [], dealer, []});

deal(Shoe, {player, PlayerHand, dealer, DealerHand}) when (length(PlayerHand) == 2) and (length(DealerHand) == 2) ->
  complete_hand(Shoe, player, {player, PlayerHand, dealer, DealerHand});

deal(Shoe, {player, PlayerHand, dealer, DealerHand}) when (length(PlayerHand)) =< (length(DealerHand)) ->
  CardNumInShoe = length(PlayerHand) + length(DealerHand) + 1,
  deal(Shoe, {player, PlayerHand ++ [shoe:deal(Shoe, CardNumInShoe)], dealer, DealerHand});

deal(Shoe, {player, PlayerHand, dealer, DealerHand}) when (length(PlayerHand)) > (length(DealerHand)) ->
  CardNumInShoe = length(PlayerHand) + length(DealerHand) + 1,
  deal(Shoe, {player, PlayerHand, dealer, DealerHand ++ [shoe:deal(Shoe, CardNumInShoe)]}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

complete_hand(Shoe, player, {player, PlayerHand, dealer, DealerHand}) ->
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

complete_hand(Shoe, dealer, {player, PlayerHand, dealer, DealerHand}) ->
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

