-module(aoc2017_day03).

-behavior(aoc_puzzle).

-export([parse/1, solve1/1, solve2/1, info/0]).

-include("aoc_puzzle.hrl").

-spec info() -> aoc_puzzle().
info() ->
    #aoc_puzzle{module = ?MODULE,
                year = 2017,
                day = 3,
                name = "Spiral Memory",
                expected = {326, 363010},
                has_input_file = false}.

-type input_type() :: integer().
-type result1_type() :: integer().
-type result2_type() :: result1_type().

-spec parse(Input :: binary()) -> input_type().
parse(_Input) ->
    361527.

-spec solve1(Input :: input_type()) -> result1_type().
solve1(Input) ->
    aoc2017_day03_part1:spiral(Input).

-spec solve2(Input :: input_type()) -> result2_type().
solve2(Input) ->
    aoc2017_day03_part2:spiral(Input).
