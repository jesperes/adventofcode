-module(aoc2017_SUITE).

-include_lib("stdlib/include/assert.hrl").

-export([ all/0
        , init_per_suite/1
        , end_per_suite/1
        , day_1/1
        , day_2/1
        , day_3/1
        , day_4/1
        , day_5/1
        ]).

all() ->
  [ day_1
  , day_2
  , day_3
  , day_4
  , day_5
  ].

init_per_suite(Config) ->
  lists:keystore(start_time, 1, Config,
                 {start_time, erlang:system_time(microsecond)}).

end_per_suite(Config) ->
  {start_time, Start} = lists:keyfind(start_time, 1, Config),
  Now = erlang:system_time(microsecond),
  ct:print("Elapsed time: ~p seconds", [(Now - Start) / 1000000]).

day_1(_Config) -> ?assertEqual(ok, eunit:test(aoc2017_day01, [verbose])).
day_2(_Config) -> ?assertEqual(ok, eunit:test(aoc2017_day02, [verbose])).
day_3(_Config) ->
  ?assertEqual(ok, eunit:test(aoc2017_day03_part1, [verbose])),
  ?assertEqual(ok, eunit:test(aoc2017_day03_part2, [verbose])).
day_4(_Config) -> ?assertEqual(ok, eunit:test(aoc2017_day04, [verbose])).
day_5(_Config) -> ?assertEqual(ok, eunit:test(aoc2017_day05, [verbose])).
