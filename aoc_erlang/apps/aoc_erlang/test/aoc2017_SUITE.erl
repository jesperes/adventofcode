-module(aoc2017_SUITE).

-include_lib("stdlib/include/assert.hrl").

-export([ all/0
        , day_1/1
        , day_2/1
        , day_3/1
        , day_4/1
        ]).

all() ->
  [ day_1
  , day_2
  , day_3
  , day_4
  ].

day_1(_Config) -> ?assertEqual(ok, eunit:test(aoc2017_day01, [verbose])).
day_2(_Config) -> ?assertEqual(ok, eunit:test(aoc2017_day02, [verbose])).
day_3(_Config) ->
  ?assertEqual(ok, eunit:test(aoc2017_day03_part1, [verbose])),
  ?assertEqual(ok, eunit:test(aoc2017_day03_part2, [verbose])).
day_4(_Config) ->
  ?assertEqual(ok, eunit:test(aoc2017_day04, [verbose])).
