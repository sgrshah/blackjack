-module(dealer).
-export([deal/1]).

% Deal 2 hands given a Shoe.
deal(Shoe) ->
  deal(Shoe, {player, [], dealer, []}).

% Each player has two cards, can begin play.
deal(Shoe, {player, PlayerHand, dealer, DealerHand}) when (length(PlayerHand) == 2) and (length(DealerHand) == 2) ->
  {player, PlayerHand, dealer, DealerHand};

% Deal to player.
deal(Shoe, {player, PlayerHand, dealer, DealerHand}) when (length(PlayerHand)) =< (length(DealerHand)) ->
  CardNumInShoe = length(PlayerHand) + length(DealerHand) + 1,
  deal(Shoe, {player, PlayerHand ++ [shoe:deal(Shoe, CardNumInShoe)], dealer, DealerHand});

% Deal to dealer.
deal(Shoe, {player, PlayerHand, dealer, DealerHand}) when (length(PlayerHand)) > (length(DealerHand)) ->
  CardNumInShoe = length(PlayerHand) + length(DealerHand) + 1,
  deal(Shoe, {player, PlayerHand, dealer, DealerHand ++ [shoe:deal(Shoe, CardNumInShoe)]}).
