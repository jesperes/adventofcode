%%% Advent of Code solution for 2019 day 09.
%%% Created: 2019-12-09T05:18:15+00:00

-module(aoc2019_day09).
-include_lib("eunit/include/eunit.hrl").


%% Puzzle solution
part1(Input) ->
  {_, [Out]} = intcode:execute(Input, [1]),
  Out.

part2(Input) ->
  {_, [Out]} = intcode:execute(Input, [2]),
  Out.

%% --- [ Input/parsing ] ---

get_input() ->
  inputs:get_as_string(2019, 09).

%% --- [ Tests ] ---

main_test_() ->
  Input = intcode:parse(get_input()),

  [ {"Part 1", ?_assertEqual(2594708277, part1(Input))}
  , {"Part 2", ?_assertEqual(87721, part2(Input))}
  ].

ex1_test_() ->
  Prog = intcode:parse("109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99"),
  Output = lists:reverse([109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99]),
  ?_assertMatch({_, Output}, intcode:execute(Prog, [0])).

ex2_test_() ->
  Prog = intcode:parse("104,1125899906842624,99"),
  ?_assertMatch({_, [1125899906842624]}, intcode:execute(Prog, 0)).

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
