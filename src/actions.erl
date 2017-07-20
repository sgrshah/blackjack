-module(actions).
-export([decide/3, decide/4]).

decide(Shoe, player, {player, PlayerHand, dealer, DealerHand}, TestType) ->
  PlayerCount = hand:sum(PlayerHand),
  io:format("PlayerCount:~p~n", [PlayerCount]),

  if
    PlayerCount < 17 ->
      io:format("TestType:~p~n", [TestType]),
      case TestType of
        hit -> hit(Shoe, player, {player, PlayerHand, dealer, DealerHand}, TestType);
        double -> double(Shoe, player, {player, PlayerHand, dealer, DealerHand});
        %% split -> split(Shoe, player, {player, PlayerHand, dealer, DealerHand});
        stand -> game:play(Shoe, dealer, {player, PlayerHand, dealer, DealerHand})
      end;
    PlayerCount > 21 ->
      io:format("player busts~n"),
      {player, PlayerHand, dealer, DealerHand};
    (PlayerCount >= 17) and (PlayerCount =< 21) ->
      io:format("player decides to stay.~n"),
      game:play(Shoe, dealer, {player, PlayerHand, dealer, DealerHand})
  end.

decide(Shoe, dealer, {player, PlayerHand, dealer, DealerHand}) ->
  DealerCount = hand:sum(DealerHand),
  io:format("DealerCount:~p~n", [DealerCount]),

  if
    DealerCount < 17 ->
      hit(Shoe, dealer, {player, PlayerHand, dealer, DealerHand});
    DealerCount > 21 ->
      io:format("dealer busts~n"),
      {player, PlayerHand, dealer, DealerHand};
    (DealerCount >= 17) and (DealerCount =< 21) ->
      io:format("dealer decides to stay.~n"),
      {player, PlayerHand, dealer, DealerHand}
  end.

hit(Shoe, player, {player, PlayerHand, dealer, DealerHand}, TestType) ->
  io:format("player decides to hit~n"),
  CardNumInShoe = length(PlayerHand) + length(DealerHand) + 1,
  NewGameState = {player, PlayerHand ++ [shoe:deal(Shoe, CardNumInShoe)], dealer, DealerHand},
  decide(Shoe, player, NewGameState, TestType).
hit(Shoe, dealer, {player, PlayerHand, dealer, DealerHand}) ->
  io:format("dealer decides to hit~n"),
  CardNumInShoe = length(PlayerHand) + length(DealerHand) + 1,
  NewGameState = {player, PlayerHand, dealer, DealerHand ++ [shoe:deal(Shoe, CardNumInShoe)]},
  decide(Shoe, dealer, NewGameState).

double(Shoe, player, {player, PlayerHand, dealer, DealerHand}) ->
  io:format("player decides to double~n"),
  CardNumInShoe = length(PlayerHand) + length(DealerHand) + 1,
  NewGameState = {player, PlayerHand ++ [shoe:deal(Shoe, CardNumInShoe)], dealer, DealerHand},
  game:play(Shoe, dealer, NewGameState).

%% split(Shoe, player, {player, [S,S], dealer, DealerHand}) ->
%%   io:format("split~p~n", [S]),
%%   io:format("player decides to split.~n");
%% split(_,_,_) ->
%%   io:format("can't split~n").
