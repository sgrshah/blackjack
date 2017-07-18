-module(hand).
-export([sum/1]).

sum(Hand) ->
  sum(Hand, 0).
sum([], Count) ->
  io:format("Sum: ~p~n", [Count]),
  Count;
sum([{1,11}|T], Count) ->
  SumWithEleven = sum(T, Count + 11),
  if
    SumWithEleven > 21 -> sum(T, Count + 1);
    true -> SumWithEleven
  end;
sum([H|T], Count) ->
  sum(T, Count + H).

