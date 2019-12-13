-module(aoc2019_SUITE).

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
  , day_13
  ].

day_1(_Config) -> ?assertEqual(ok, eunit:test(aoc2019_day01, [verbose])).
day_2(_Config) -> ?assertEqual(ok, eunit:test(aoc2019_day02, [verbose])).
day_3(_Config) -> ?assertEqual(ok, eunit:test(aoc2019_day03, [verbose])).
day_4(_Config) -> ?assertEqual(ok, eunit:test(aoc2019_day04, [verbose])).
day_5(_Config) -> ?assertEqual(ok, eunit:test(aoc2019_day05, [verbose])).
day_6(_Config) -> ?assertEqual(ok, eunit:test(aoc2019_day06, [verbose])).
day_7(_Config) ->
  ?assertEqual(ok, eunit:test(aoc2019_day07, [verbose])),
  ?assertEqual(ok, eunit:test(aoc2019_day07_part2, [verbose])).
day_8(_Config) -> ?assertEqual(ok, eunit:test(aoc2019_day08, [verbose])).
day_9(_Config) -> ?assertEqual(ok, eunit:test(aoc2019_day09, [verbose])).
day_10(_Config) -> ?assertEqual(ok, eunit:test(aoc2019_day10, [verbose])).
day_11(_Config) -> ?assertEqual(ok, eunit:test(aoc2019_day11, [verbose])).
day_12(_Config) -> ?assertEqual(ok, eunit:test(aoc2019_day12, [verbose])).
day_13(_Config) -> ?assertEqual(ok, eunit:test(aoc2019_day13, [verbose])).
