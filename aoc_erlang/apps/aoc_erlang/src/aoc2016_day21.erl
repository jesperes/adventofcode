%%% Advent of Code solution for 2016 day 21.
%%% Created: 2019-11-23T19:15:30+00:00

-module(aoc2016_day21).
-include_lib("eunit/include/eunit.hrl").

%% Puzzle solution
part1(_Input) ->
  ?debugMsg("Not implemented."),
  0.

part2(_Input) ->
  ?debugMsg("Not implemented."),
  0.

%% Input reader (place downloaded input file in
%% priv/inputs/2016/input21.txt).
get_input() ->
  inputs:get_as_binary(2016, 21).

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
