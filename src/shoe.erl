-module(shoe).
-export([create/1, deal/2]).

% This module encapsulates all logic related to the shoe.

create({decks, 1}) ->
  Shoe = [
    2, 2, 2, 2,
    3, 3, 3, 3,
    4, 4, 4, 4,
    5, 5, 5, 5,
    6, 6, 6, 6,
    7, 7, 7, 7,
    8, 8, 8, 8,
    9, 9, 9, 9,
    10, 10, 10, 10, % 10s
    10, 10, 10, 10, % Js
    10, 10, 10, 10, % Qs
    10, 10, 10, 10, % Ks
    {1,11}, {1,11}, {1,11}, {1,11} % As
  ],
  shuffle(Shoe).

shuffle(Shoe) ->
  [Card || {_, Card} <- lists:sort([{rand:uniform(), N} || N <- Shoe])].

deal(Shoe, CardNum) ->
  deal(Shoe, CardNum, 1).
deal([_|T], CardNum, Count) when Count < CardNum ->
  deal(T, CardNum, Count + 1);
deal([H|_], CardNum, Count) when Count == CardNum ->
  H.
