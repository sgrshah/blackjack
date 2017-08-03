-module(game).
-behavior(gen_statem).
% External API
-export([start_link/0, trigger/1]).
% gen_statem specific
-export([init/1, callback_mode/0]).
% state functions
-export([player_turn/3,
         dealer_turn/3,
         game_evaluation/3,
         blackjack/3,
         player_bust/3,
         dealer_bust/3,
         player_win/3,
         dealer_win/3,
         push/3]).

% game state machine should die if game server dies.
start_link() ->
    gen_statem:start_link(?MODULE, [], []).

% Initialize with a shoe and RANDOM hand if no instructions passed.
init([]) ->
  Shoe = shoe:create({decks, 1}),
  InitialHands = dealer:deal(Shoe),
  {ok, player_turn, {Shoe, InitialHands}};

% Initialize with a predfined hand with portions of shoe removed.
init(InitialHands) ->
  % Remove cards from Shoe
  {ok, player_turn, [], {}}.

% External API call to trigger state changes
trigger(Pid) ->
  gen_statem:call(Pid, {trigger, hit}).

% This specifies that I want separate functions for each state. The other option is a single event
% handler.
callback_mode() ->
  [state_functions].

% State Functions
% These functions get called after gen_statem:call depending on the state of the machine.

player_turn({call, From}, {_, TestType}, {Shoe, Hands}) ->
  {player, PlayerHand, dealer, _} = Hands,
  PlayerHandCount = hand:sum(PlayerHand),

  if
    (length(PlayerHand) == 2) and (PlayerHandCount == 21) ->
      {next_state, blackjack, {Shoe, Hands}, [{reply, From, {blackjack, Hands, 0}}]};
    (PlayerHandCount >= 17) and (PlayerHandCount =< 21) ->
      {next_state, dealer_turn, {Shoe, Hands}, [{reply, From, {dealer_turn, Hands, 0}}]};
    (PlayerHandCount < 17) ->
      case TestType of
        hit ->
          NewHands = hit(Shoe, player, Hands),
          {keep_state, {Shoe, NewHands}, [{reply, From, {player_turn, NewHands, 0}}]};
        double ->
          NewHands = hit(Shoe, player, Hands),
          {next_state, dealer_turn, {Shoe, NewHands}, [{reply, From, {dealer_turn, NewHands, 0}}]};
        stand ->
          {next_state, dealer_turn, {Shoe, Hands}, [{reply, From, {dealer_turn, Hands, 0}}]}
      end;
    (PlayerHandCount > 21) ->
      {next_state, player_bust, {Shoe, Hands}, [{reply, From, {player_bust, Hands, 0}}]}
  end.

dealer_turn({call, From}, {_, TestType}, {Shoe, Hands}) ->
  {player, _, dealer, DealerHand} = Hands,
  DealerCount = hand:sum(DealerHand),

  if
    (DealerCount < 17) ->
      NewHands = hit(Shoe, dealer, Hands),
      {keep_state, {Shoe, NewHands}, [{reply, From, {dealer_turn, NewHands, 0}}]};
    (DealerCount > 21) ->
      {next_state, dealer_bust, {Shoe, Hands}, [{reply, From, {dealer_bust, Hands, 0}}]};
    (DealerCount >= 17) and (DealerCount =< 21) ->
      {next_state, game_evaluation, {Shoe, Hands}, [{reply, From, {game_evaluation, Hands, 0}}]}
  end.

blackjack({call, From}, _, {Shoe, Hands}) ->
  {next_state, player_win, {Shoe, Hands}, [{reply, From, {player_win, Hands, 1}}]}.

player_bust({call, From}, _, {Shoe, Hands}) ->
  {next_state, dealer_win, {Shoe, Hands}, [{reply, From, {dealer_win, Hands, -1}}]}.

dealer_bust({call, From}, _, {Shoe, Hands}) ->
  {next_state, player_win, {Shoe, Hands}, [{reply, From, {player_win, Hands, 1}}]}.

game_evaluation({call, From}, _, {Shoe, {_, PlayerHand, _, DealerHand}}) when PlayerHand > DealerHand ->
  Hands = {player, PlayerHand, dealer, DealerHand},
  {next_state, player_win, {Shoe, Hands}, [{reply, From, {player_win, Hands, 1}}]};

game_evaluation({call, From}, _, {Shoe, {_, PlayerHand, _, DealerHand}}) when PlayerHand < DealerHand ->
  Hands = {player, PlayerHand, dealer, DealerHand},
  {next_state, dealer_win, {Shoe, Hands}, [{reply, From, {dealer_win, Hands, -1}}]};

game_evaluation({call, From}, _, {Shoe, {_, PlayerHand, _, DealerHand}}) when PlayerHand == DealerHand ->
  Hands = {player, PlayerHand, dealer, DealerHand},
  {next_state, push, {Shoe, Hands}, [{reply, From, {push, Hands, 0}}]}.

player_win({call, From}, _, {Shoe, Hands}) ->
  {keep_state, {Shoe, Hands}, [{reply, From, {player_win, Hands, 1}}]}.

dealer_win({call, From}, _, {Shoe, Hands}) ->
  {keep_state, {Shoe, Hands}, [{reply, From, {dealer_win, Hands, -1}}]}.

push({call, From}, _, {Shoe, Hands}) ->
  {keep_state, {Shoe, Hands}, [{reply, From, {push, Hands, 0}}]}.

% Private Methods

hit(Shoe, player, {player, PlayerHand, dealer, DealerHand}) ->
  CardNumInShoe = length(PlayerHand) + length(DealerHand) + 1,
  {player, PlayerHand ++ [shoe:deal(Shoe, CardNumInShoe)], dealer, DealerHand};
hit(Shoe, dealer, {player, PlayerHand, dealer, DealerHand}) ->
  CardNumInShoe = length(PlayerHand) + length(DealerHand) + 1,
  {player, PlayerHand, dealer, DealerHand ++ [shoe:deal(Shoe, CardNumInShoe)]}.

