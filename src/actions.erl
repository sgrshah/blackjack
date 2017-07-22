-module(actions).
-export([decide/3, decide/4]).

decide(Shoe, player, {player, PlayerHand, dealer, DealerHand}, TestType) ->
  PlayerCount = hand:sum(PlayerHand),

  if
    PlayerCount < 17 ->
      case TestType of
        hit -> hit(Shoe, player, {player, PlayerHand, dealer, DealerHand}, TestType);
        double -> double(Shoe, player, {player, PlayerHand, dealer, DealerHand});
        stand -> game:play(Shoe, dealer, {player, PlayerHand, dealer, DealerHand})
      end;
    PlayerCount > 21 ->
      {player, PlayerHand, dealer, DealerHand};
    (PlayerCount >= 17) and (PlayerCount =< 21) ->
      game:play(Shoe, dealer, {player, PlayerHand, dealer, DealerHand})
  end.

decide(Shoe, dealer, {player, PlayerHand, dealer, DealerHand}) ->
  DealerCount = hand:sum(DealerHand),

  if
    DealerCount < 17 ->
      hit(Shoe, dealer, {player, PlayerHand, dealer, DealerHand});
    DealerCount > 21 ->
      {player, PlayerHand, dealer, DealerHand};
    (DealerCount >= 17) and (DealerCount =< 21) ->
      {player, PlayerHand, dealer, DealerHand}
  end.

hit(Shoe, player, {player, PlayerHand, dealer, DealerHand}, TestType) ->
  CardNumInShoe = length(PlayerHand) + length(DealerHand) + 1,
  NewGameState = {player, PlayerHand ++ [shoe:deal(Shoe, CardNumInShoe)], dealer, DealerHand},
  decide(Shoe, player, NewGameState, TestType).
hit(Shoe, dealer, {player, PlayerHand, dealer, DealerHand}) ->
  CardNumInShoe = length(PlayerHand) + length(DealerHand) + 1,
  NewGameState = {player, PlayerHand, dealer, DealerHand ++ [shoe:deal(Shoe, CardNumInShoe)]},
  decide(Shoe, dealer, NewGameState).

double(Shoe, player, {player, PlayerHand, dealer, DealerHand}) ->
  CardNumInShoe = length(PlayerHand) + length(DealerHand) + 1,
  NewGameState = {player, PlayerHand ++ [shoe:deal(Shoe, CardNumInShoe)], dealer, DealerHand},
  game:play(Shoe, dealer, NewGameState).

