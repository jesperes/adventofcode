%%% Advent of Code solution for 2019 day 02.
%%% Created: 2019-12-02T08:58:25+00:00

-module(aoc2019_day02).
-include_lib("eunit/include/eunit.hrl").

%% Puzzle solution
part1(Prog) ->
  run_intcode(Prog, 12, 2).

part2(Input) ->
  [100 * A + B || A <- lists:seq(0, 99),
                  B <- lists:seq(0, 99),
                  run_intcode(Input, A, B) =:= 19690720].

run_intcode(Prog, A, B) ->
  {ProgOut, _} =
    intcode:execute(maps:merge(Prog, #{1 => A, 2 => B})),
  maps:get(0, ProgOut).

%% Tests
main_test_() ->
  Input = intcode:parse(inputs:get_as_binary(2019, 2)),

  [ {"Part 1", ?_assertEqual(3654868, part1(Input))}
  , {"Part 2", ?_assertEqual([7014], part2(Input))}
  ].

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
