-module(aoc2020_SUITE).

-include_lib("stdlib/include/assert.hrl").

-compile([export_all, nowarn_export_all]).

tc({F, _}) ->
  case re:run(atom_to_list(F), "day_.*") of
    {match, _} -> {true, F};
    _ -> false
  end.

all() ->
  lists:filtermap(fun tc/1, ?MODULE:module_info(exports)).

day_1(_Config) -> ?assertEqual(ok, eunit:test(aoc2020_day01, [verbose])).
day_2(_Config) -> ?assertEqual(ok, eunit:test(aoc2020_day02, [verbose])).
day_3(_Config) -> ?assertEqual(ok, eunit:test(aoc2020_day03, [verbose])).
day_4(_Config) -> ?assertEqual(ok, eunit:test(aoc2020_day04, [verbose])).
day_5(_Config) -> ?assertEqual(ok, eunit:test(aoc2020_day05, [verbose])).
day_6(_Config) -> ?assertEqual(ok, eunit:test(aoc2020_day06, [verbose])).
day_7(_Config) -> ?assertEqual(ok, eunit:test(aoc2020_day07, [verbose])).
day_8(_Config) -> ?assertEqual(ok, eunit:test(aoc2020_day08, [verbose])).
day_9(_Config) -> ?assertEqual(ok, eunit:test(aoc2020_day09, [verbose])).
day_10(_Config) -> ?assertEqual(ok, eunit:test(aoc2020_day10, [verbose])).
day_11(_Config) -> ?assertEqual(ok, eunit:test(aoc2020_day11, [verbose])).
day_12(_Config) -> ?assertEqual(ok, eunit:test(aoc2020_day12, [verbose])).
day_13(_Config) -> ?assertEqual(ok, eunit:test(aoc2020_day13, [verbose])).
day_14(_Config) -> ?assertEqual(ok, eunit:test(aoc2020_day14, [verbose])).
day_15(_Config) -> ?assertEqual(ok, eunit:test(aoc2020_day15, [verbose])).
day_16(_Config) -> ?assertEqual(ok, eunit:test(aoc2020_day16, [verbose])).
day_17(_Config) -> ?assertEqual(ok, eunit:test(aoc2020_day17, [verbose])).
day_18(_Config) -> ?assertEqual(ok, eunit:test(aoc2020_day18, [verbose])).
day_19(_Config) -> ?assertEqual(ok, eunit:test(aoc2020_day19, [verbose])).
day_20(_Config) -> ?assertEqual(ok, eunit:test(aoc2020_day20, [verbose])).
day_21(_Config) -> ?assertEqual(ok, eunit:test(aoc2020_day21, [verbose])).
day_22(_Config) -> ?assertEqual(ok, eunit:test(aoc2020_day22, [verbose])).
day_23(_Config) -> ?assertEqual(ok, eunit:test(aoc2020_day23, [verbose])).
day_24(_Config) -> ?assertEqual(ok, eunit:test(aoc2020_day24, [verbose])).
day_25(_Config) -> ?assertEqual(ok, eunit:test(aoc2020_day25, [verbose])).
