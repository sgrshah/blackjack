-module(actions).
-export([decide/3]).

decide(Shoe, player, {player, PlayerHand, dealer, DealerHand}) ->
  PlayerCount = hand:sum(PlayerHand),

  if
    PlayerCount < 17 ->
      io:format("player decides to hit~n"),
      hit(Shoe, player, {player, PlayerHand, dealer, DealerHand});
    PlayerCount > 21 ->
      io:format("player busts~n"),
      {player, PlayerHand, dealer, DealerHand};
    (PlayerCount >= 17) and (PlayerCount =< 21) ->
      io:format("player decides to stay~n"),
      game:complete_hand(Shoe, dealer, {player, PlayerHand, dealer, DealerHand})
  end;

decide(Shoe, dealer, {player, PlayerHand, dealer, DealerHand}) ->
  DealerCount = hand:sum(DealerHand),

  if
    DealerCount < 17 ->
      io:format("dealer decides to hit~n"),
      hit(Shoe, dealer, {player, DealerHand, dealer, DealerHand});
    DealerCount > 21 ->
      io:format("dealer busts~n"),
      {player, PlayerHand, dealer, DealerHand};
    (DealerCount >= 17) and (DealerCount =< 21) ->
      io:format("dealer decides to stay~n"),
      {player, PlayerHand, dealer, DealerHand}
  end.

hit(Shoe, player, {player, PlayerHand, dealer, DealerHand}) ->
  io:format("player hit~n"),
  CardNumInShoe = length(PlayerHand) + length(DealerHand) + 1,
  decide(Shoe, player, {player, PlayerHand ++ [shoe:deal(Shoe, CardNumInShoe)], dealer,
                               DealerHand});
hit(Shoe, dealer, {player, PlayerHand, dealer, DealerHand}) ->
  io:format("dealer hit~n"),
  CardNumInShoe = length(PlayerHand) + length(DealerHand) + 1,
  decide(Shoe, dealer, {player, PlayerHand, dealer, DealerHand ++ [shoe:deal(Shoe, CardNumInShoe)]}).
