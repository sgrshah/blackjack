-module(shoe).
-export([create/1, deal/2]).
-define(SHOE, [
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
  ]).

% This module encapsulates all logic related to the shoe. The shoe is the container for all the
% cards, and can consist of 1-6 decks.

% Create takes an argument that specifies the number of decks in the shoe.
% It creates a list of all the card values.
% Aces are represented as tuples {1,11}.
% It shuffles the deck into a random order.
create({decks, 1}) ->
  shuffle(?SHOE).

% Shuffle uses list comprehension to randomly order a Shoe.
shuffle(Shoe) ->
  [Card || {_, Card} <- lists:sort([{rand:uniform(), N} || N <- Shoe])].

% The deal function pulls the Nth item from a shoe.
deal(Shoe, CardNum) ->
  deal(Shoe, CardNum, 1).

deal([_|T], CardNum, Count) when Count < CardNum ->
  deal(T, CardNum, Count + 1);

% When the aggregator count and specified index value are equal, the head of the list is the desired
% value.
deal([H|_], CardNum, Count) when Count == CardNum ->
  H.
