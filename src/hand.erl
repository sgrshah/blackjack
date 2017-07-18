-module(hand).
-export([sum/1]).

% Exported function that initializes accumulator to zero.
sum(Hand) ->
  sum(Hand, 0).

% Base case. Return accumulator when entire Hand has been counted.
sum([], Count) ->
  Count;

% Handle Aces, which can be 1 or 11.
sum([{1,11}|T], Count) ->
  SumWithEleven = sum(T, Count + 11),
  if
    SumWithEleven > 21 -> sum(T, Count + 1);
    true -> SumWithEleven
  end;

% Add Head to Accumulator
sum([H|T], Count) ->
  sum(T, Count + H).

