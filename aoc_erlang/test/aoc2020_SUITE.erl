-module(aoc2020_SUITE).

-include_lib("stdlib/include/assert.hrl").

-compile([export_all, nowarn_export_all]).

all() ->
  lists:filtermap(
    fun({F, _}) ->
        case re:run(atom_to_list(F), "day_.*") of
          {match, _} -> {true, F};
          _ -> false
        end
    end, ?MODULE:module_info(exports)).

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
