%%% Advent of Code solution for 2020 day 22.
%%% Created: 2020-12-22T06:17:29+00:00

-module(aoc2020_day22).
-include_lib("eunit/include/eunit.hrl").

-compile([nowarn_unused_function]).

%% Puzzle solution
part1({Deck1, Deck2}) ->
  crab_combat(Deck1, Deck2).

part2({Deck1, Deck2}) ->
  rec_crab_combat(Deck1, Deck2).

%% =======================================================================
%% Regular crab combat
%% =======================================================================

crab_combat([], Deck2) ->
  sum_deck(Deck2);
crab_combat(Deck1, []) ->
  sum_deck(Deck1);
crab_combat([D1|Deck1], [D2|Deck2]) when D1 > D2 ->
  crab_combat(Deck1 ++ [D1, D2], Deck2);
crab_combat([D1|Deck1], [D2|Deck2]) when D1 < D2 ->
  crab_combat(Deck1, Deck2 ++ [D2, D1]).

%% =======================================================================
%% Recursive crab combat
%% =======================================================================


rec_crab_combat(Deck1, Deck2) ->
  rec_crab_combat(Deck1, Deck2, sets:new()).

rec_crab_combat(Deck1, [], _Rounds) ->
  {player1, sum_deck(Deck1)};
rec_crab_combat([], Deck2, _Rounds) ->
  {player2, sum_deck(Deck2)};
rec_crab_combat(Deck1, Deck2, Rounds) ->
  State = {Deck1, Deck2},
  case sets:is_element(State, Rounds) of
    true ->
      {player1, sum_deck(Deck1)};
    false ->
      %% Remember new configuration
      Rounds0 = sets:add_element(State, Rounds),

      %% Draw cards
      [Draw1|Rest1] = Deck1,
      [Draw2|Rest2] = Deck2,

      %% If both players have at least as many cards remaining in
      %% their deck as the value of the card they just drew, the
      %% winner of the round is determined by playing a new game of
      %% Recursive Combat.
      case (Draw1 =< length(Rest1)) andalso (Draw2 =< length(Rest2)) of
        true ->
          %% The sub-game does *not* use the entire deck, but only the
          %% number specified by the drawn card...
          {L1, _} = lists:split(Draw1, Rest1),
          {L2, _} = lists:split(Draw2, Rest2),

          %% Play recursive game
          case rec_crab_combat(L1, L2, sets:new()) of
            {player1, _} ->
              rec_crab_combat(Rest1 ++ [Draw1, Draw2], Rest2, Rounds0);
            {player2, _} ->
              rec_crab_combat(Rest1, Rest2 ++ [Draw2, Draw1], Rounds0)
          end;

        false ->
          if Draw1 > Draw2 ->
              rec_crab_combat(Rest1 ++ [Draw1, Draw2], Rest2, Rounds0);
            Draw1 < Draw2 ->
              rec_crab_combat(Rest1, Rest2 ++ [Draw2, Draw1], Rounds0)
          end
      end
  end.

sum_deck(Deck) ->
  Len = length(Deck),
  lists:foldl(fun({N, D}, Acc) ->
                  N * D + Acc
              end, 0, lists:zip(lists:seq(Len, 1, -1), Deck)).

get_input() ->
  {[ 29, 21, 38, 30, 25, 7, 2, 36, 16, 44, 20, 12, 45,
     4, 31, 34, 33, 42, 50, 14, 39, 37, 11, 43, 18],
   [ 32, 24, 10, 41, 13, 3, 6, 5, 9, 8, 48, 49, 46, 17,
     22, 35, 1, 19, 23, 28, 40, 26, 47, 15, 27]}.

%% Tests
main_test_() ->
  Input = get_input(),

  [ {"Part 1", ?_assertEqual(32677, part1(Input))}
  , {"Part 2", ?_assertMatch({_, 33661}, part2(Input))}
  ].

test_input() ->
  {[9, 2, 6, 3, 1],
   [5, 8, 4, 7, 10]}.

ex1_test_() ->
  {Deck1, Deck2} = test_input(),
  ?_assertEqual(306, crab_combat(Deck1, Deck2)).

ex2_test_() ->
  {Deck1, Deck2} = test_input(),
  ?_assertMatch({_, 291}, rec_crab_combat(Deck1, Deck2)).

ex2b_test_() ->
  Deck1 = [43, 19],
  Deck2 = [2, 29, 14],
  %% This is just to test termination (so no timeout here!)
  fun() -> rec_crab_combat(Deck1, Deck2) end.

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
