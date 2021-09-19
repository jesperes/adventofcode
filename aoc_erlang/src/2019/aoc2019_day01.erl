%%% Advent of Code solution for 2019 day 01.
%%% Created: 2019-12-01T20:46:36+00:00

-module(aoc2019_day01).
-include_lib("eunit/include/eunit.hrl").

part1(Input) ->
  lists:foldl(fun(M, A) -> A + (M div 3 - 2) end, 0, Input).

part2(Input) ->
  lists:foldl(fun(M, A) -> A + fuel(M) end, 0, Input).

fuel(M) ->
  F = M div 3 - 2,
  case F =< 0 of
    true -> 0;
    false -> F + fuel(F)
  end.

get_input() ->
  inputs:get_as_ints(2019, 1).

%% Tests
main_test_() ->
  Input = get_input(),

  [ {"Part 1", ?_assertEqual(3368364, part1(Input))}
  , {"Part 2", ?_assertEqual(5049684, part2(Input))}
  ].

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
