-module(aoc2018_day05).
-include_lib("eunit/include/eunit.hrl").

main_test_() ->
  Input = string:trim(inputs:get_as_string(2018, 5)),
  [ {"Part 1", ?_assertEqual(10496, start1(Input))}
  , {"Part 2", ?_assertEqual(5774, start2(Input))}
  ].

start1(L) ->
  length(react(L)).

start2(L) ->
  lists:min([start1([C || C <- L, C =/= X, C =/= X - 32]) ||
              X <- lists:seq($a, $z)]).

react(L) ->
  react(L, []).

react([], L) ->
  L;
react([C1|L1], [C2|L2]) when abs(C1 - C2) == 32 ->
  react(L1, L2);
react([C|L1], L2) ->
  react(L1, [C|L2]).
