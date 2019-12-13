-module(aoc2018_SUITE).

-include_lib("stdlib/include/assert.hrl").

-export([ all/0
        , day_1/1
        , day_2/1
        , day_3/1
        , day_4/1
        , day_5/1
        , day_6/1
        , day_7/1
        , day_8/1
        , day_9/1
        , day_10/1
        , day_11/1
        , day_12/1
        , day_13/1
        , day_14/1
        , day_15/1
        , day_16/1
        , day_17/1
        , day_18/1
        , day_19/1
        , day_20/1
        , day_21/1
        , day_22/1
        , day_23/1
        , day_24/1
        , day_25/1
        ]).

all() ->
  [ day_1
  , day_2
  , day_3
  , day_4
  , day_5
  , day_6
  , day_7
  , day_8
  , day_9
  , day_10
  , day_11
  , day_12
  , day_13
  , day_14
  , day_15
  , day_16
  , day_17
  , day_18
  , day_19
  , day_20
  , day_21
  , day_22
  , day_23
  , day_24
  , day_25
  ].

day_1(_Config) -> ?assertEqual(ok, eunit:test(aoc2018_day01, [verbose])).
day_2(_Config) -> ?assertEqual(ok, eunit:test(aoc2018_day02, [verbose])).
day_3(_Config) -> ?assertEqual(ok, eunit:test(aoc2018_day03, [verbose])).
day_4(_Config) -> ?assertEqual(ok, eunit:test(aoc2018_day04, [verbose])).
day_5(_Config) -> ?assertEqual(ok, eunit:test(aoc2018_day05, [verbose])).
day_6(_Config) -> ?assertEqual(ok, eunit:test(aoc2018_day06, [verbose])).
day_7(_Config) -> ?assertEqual(ok, eunit:test(aoc2018_day07, [verbose])).
day_8(_Config) -> ?assertEqual(ok, eunit:test(aoc2018_day08, [verbose])).
day_9(_Config) -> ?assertEqual(ok, eunit:test(aoc2018_day09, [verbose])).
day_10(_Config) -> ?assertEqual(ok, eunit:test(aoc2018_day10, [verbose])).
day_11(_Config) -> ?assertEqual(ok, eunit:test(aoc2018_day11, [verbose])).
day_12(_Config) -> ?assertEqual(ok, eunit:test(aoc2018_day12, [verbose])).
day_13(_Config) -> ?assertEqual(ok, eunit:test(aoc2018_day13, [verbose])).
day_14(_Config) -> ?assertEqual(ok, eunit:test(aoc2018_day14, [verbose])).
day_15(_Config) -> ?assertEqual(ok, eunit:test(aoc2018_day15, [verbose])).
day_16(_Config) -> ?assertEqual(ok, eunit:test(aoc2018_day16, [verbose])).
day_17(_Config) -> ?assertEqual(ok, eunit:test(aoc2018_day17, [verbose])).
day_18(_Config) -> ?assertEqual(ok, eunit:test(aoc2018_day18, [verbose])).
day_19(_Config) -> ?assertEqual(ok, eunit:test(aoc2018_day19, [verbose])).
day_20(_Config) -> ?assertEqual(ok, eunit:test(aoc2018_day20, [verbose])).
day_21(_Config) -> ?assertEqual(ok, eunit:test(aoc2018_day21, [verbose])).
day_22(_Config) -> ?assertEqual(ok, eunit:test(aoc2018_day22, [verbose])).
day_23(_Config) -> ?assertEqual(ok, eunit:test(aoc2018_day23, [verbose])).
day_24(_Config) -> ?assertEqual(ok, eunit:test(aoc2018_day24, [verbose])).
day_25(_Config) -> ?assertEqual(ok, eunit:test(aoc2018_day25, [verbose])).
