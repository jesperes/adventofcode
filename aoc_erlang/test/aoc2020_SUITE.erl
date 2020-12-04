-module(aoc2020_SUITE).

-include_lib("stdlib/include/assert.hrl").

-export([ all/0
        , day_1/1
        , day_2/1
        , day_3/1
        , day_4/1
        %% , day_5/1
        %% , day_6/1
        %% , day_7/1
        %% , day_8/1
        %% , day_9/1
        %% , day_10/1
        %% , day_11/1
        %% , day_12/1
        %% , day_13/1
        %% , day_14/1
        %% , day_15/1
        %% , day_16/1
        %% , day_17/1
        %% , day_18/1
        %% , day_19/1
        %% , day_20/1
        %% , day_21/1
        %% , day_22/1
        %% , day_23/1
        %% , day_24/1
        %% , day_25/1
        ]).

all() ->
  [ day_1
  , day_2
  , day_3
  , day_4
  %% , day_5
  %% , day_6
  %% , day_7
  %% , day_8
  %% , day_9
  %% , day_10
  %% , day_11
  %% , day_12
  %% , day_13
  %% , day_14
  %% , day_15
  %% , day_16
  %% , day_17
  %% , day_18
  %% , day_19
  %% , day_20
  %% , day_21
  %% , day_22
  %% , day_23
  %% , day_24
  %% , day_25
  ].

day_1(_Config) -> ?assertEqual(ok, eunit:test(aoc2020_day01, [verbose])).
day_2(_Config) -> ?assertEqual(ok, eunit:test(aoc2020_day02, [verbose])).
day_3(_Config) -> ?assertEqual(ok, eunit:test(aoc2020_day03, [verbose])).
day_4(_Config) -> ?assertEqual(ok, eunit:test(aoc2020_day04, [verbose])).
