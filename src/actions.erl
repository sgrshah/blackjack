-module(actions).
-export([decide/3]).

decide(Shoe, player, {player, PlayerHand, dealer, DealerHand}) ->
  PlayerCount = hand:sum(PlayerHand),
  io:format("PlayerCount:~p~n", [PlayerCount]),

  if
    PlayerCount < 17 ->
      io:format("player decides to hit~n"),
      hit(Shoe, player, {player, PlayerHand, dealer, DealerHand});
    PlayerCount > 21 ->
      io:format("player busts~n"),
      {player, PlayerHand, dealer, DealerHand};
    (PlayerCount >= 17) and (PlayerCount =< 21) ->
      io:format("player decides to stay.~n"),
      game:play(Shoe, dealer, {player, PlayerHand, dealer, DealerHand})
  end;

decide(Shoe, dealer, {player, PlayerHand, dealer, DealerHand}) ->
  DealerCount = hand:sum(DealerHand),
  io:format("DealerCount:~p~n", [DealerCount]),

  if
    DealerCount < 17 ->
      io:format("dealer decides to hit~n"),
      hit(Shoe, dealer, {player, PlayerHand, dealer, DealerHand});
    DealerCount > 21 ->
      io:format("dealer busts~n"),
      {player, PlayerHand, dealer, DealerHand};
    (DealerCount >= 17) and (DealerCount =< 21) ->
      io:format("dealer decides to stay.~n"),
      {player, PlayerHand, dealer, DealerHand}
  end.

hit(Shoe, player, {player, PlayerHand, dealer, DealerHand}) ->
  CardNumInShoe = length(PlayerHand) + length(DealerHand) + 1,
  NewGameState = {player, PlayerHand ++ [shoe:deal(Shoe, CardNumInShoe)], dealer, DealerHand},
  decide(Shoe, player, NewGameState);
hit(Shoe, dealer, {player, PlayerHand, dealer, DealerHand}) ->
  CardNumInShoe = length(PlayerHand) + length(DealerHand) + 1,
  NewGameState = {player, PlayerHand, dealer, DealerHand ++ [shoe:deal(Shoe, CardNumInShoe)]},
  decide(Shoe, dealer, NewGameState).
