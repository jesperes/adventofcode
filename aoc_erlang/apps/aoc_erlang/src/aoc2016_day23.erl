%%% Advent of Code solution for 2016 day 23.
%%% Created: 2019-11-24T07:51:57+00:00

-module(aoc2016_day23).
-include_lib("eunit/include/eunit.hrl").

%% Puzzle solution
part1(_Input) ->
  ?debugMsg("Not implemented."),
  0.

part2(_Input) ->
  ?debugMsg("Not implemented."),
  0.

%% Input reader (place downloaded input file in
%% priv/inputs/2016/input23.txt).
get_input() ->
  inputs:get_as_binary(2016, 23).

%% Tests
main_test_() ->
  Input = get_input(),

  [ {"Part 1", ?_assertEqual(0, part1(Input))}
  , {"Part 2", ?_assertEqual(0, part2(Input))}
  ].

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
