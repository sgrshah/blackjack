-module(game).
-behavior(gen_server).

% External API
-export([play/3, play/4]).

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
