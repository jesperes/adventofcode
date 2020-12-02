%%% Advent of Code solution for 2019 day 21.
%%% Created: 2019-12-21T06:44:20+00:00

-module(aoc2019_day21).
-include_lib("eunit/include/eunit.hrl").

part1(Prog) ->
  Springcode =
    "OR A J\n"
    "AND B J\n"
    "AND C J\n"
    "NOT J J\n"
    "AND D J\n"
    "WALK\n",

  {_, [Damage|_]} = intcode:execute(Prog, Springcode),
  Damage.

part2(Prog) ->
  Springcode =
    "OR A J\n"
    "AND B J\n"
    "AND C J\n"
    "NOT J J\n"
    "AND D J\n"
    "OR E T\n"
    "OR H T\n"
    "AND T J\n"
    "RUN\n",

  {_, [Damage|_]} = intcode:execute(Prog, Springcode),
  Damage.

get_input() ->
  intcode:parse(inputs:get_as_binary(2019, 21)).

%% Tests
main_test_() ->
  Input = get_input(),

  [ {"Part 1", ?_assertEqual(19354437, part1(Input))}
  , {"Part 2", ?_assertEqual(1145373084, part2(Input))}
  ].

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
