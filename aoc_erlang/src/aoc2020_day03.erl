%%% Advent of Code solution for 2020 day 03.
%%% Created: 2020-12-03T07:01:04+00:00

-module(aoc2020_day03).
-include_lib("eunit/include/eunit.hrl").

%% Puzzle solution
part1(Input) ->
  num_trees(Input, {3, 1}).

part2(Input) ->
  num_trees(Input, {1, 1}) *
    num_trees(Input, {3, 1}) *
    num_trees(Input, {5, 1}) *
    num_trees(Input, {7, 1}) *
    num_trees(Input, {1, 2}).

num_trees(Lines, Slope) ->
  num_trees(Lines, 0, 0, 0, Slope).

num_trees(Lines, _X, Y, NumTrees, _Slope)
  when Y >= length(Lines) ->
  NumTrees;
num_trees(Lines, X, Y, NumTrees, {SlopeX, SlopeY} = Slope) ->
  Line = lists:nth(Y + 1, Lines),
  Width = length(Line),
  NumTrees1 =
    case lists:nth((X rem Width) + 1, Line) of
      $# -> NumTrees + 1;
      _ -> NumTrees
    end,
  num_trees(Lines, X + SlopeX, Y + SlopeY, NumTrees1, Slope).

%% Input reader (place downloaded input file in
%% priv/inputs/2020/input03.txt).
get_input() ->
  inputs:get_as_lines(2020, 03).

%% Tests
main_test_() ->
  Input = get_input(),

  [ {"Part 1", ?_assertEqual(230, part1(Input))}
  , {"Part 2", ?_assertEqual(9533698720, part2(Input))}
  ].

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
