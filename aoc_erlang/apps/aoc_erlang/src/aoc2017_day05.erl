%%% Advent of Code solution for 2017 day 05.
%%% Created: 2019-12-11T20:54:07+00:00

-module(aoc2017_day05).
-include_lib("eunit/include/eunit.hrl").

%% Puzzle solution
part1(Offsets) ->
  execute(Offsets, 0, 0, fun(N) -> N + 1 end).

part2(Offsets) ->
  execute(Offsets, 0, 0,
          fun(N) when N >= 3 -> N - 1;
             (N) -> N + 1
          end).

execute(Offsets, PC, Steps, Fun) ->
  case maps:get(PC, Offsets, eop) of
    eop -> Steps;
    N -> execute(maps:update_with(PC, Fun, Offsets),
                 PC + N, Steps + 1, Fun)
  end.

get_input() ->
  Ints = inputs:get_as_ints(2017, 05),
  to_map(Ints).

to_map(Ints) ->
  Indexes = lists:seq(0, length(Ints)-1),
  maps:from_list(lists:zip(Indexes, Ints)).


%% Tests
main_test_() ->
  Offsets = get_input(),

  [ {"Part 1", ?_assertEqual(339351, part1(Offsets))}
  , {"Part 2", timeout, 60, ?_assertEqual(24315397, part2(Offsets))}
  ].

ex1_test_() ->
  Offsets = to_map([0, 3, 0, 1, -3]),
  [ ?_assertEqual(5, part1(Offsets))
  , ?_assertEqual(10, part2(Offsets))
  ].

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
