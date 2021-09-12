-module(aoc_template2).

-behavior(aoc_puzzle).

-export([parse/1, solve1/1, solve2/1, info/0]).

-include("aoc_puzzle.hrl").

-spec info() -> aoc_puzzle().
info() ->
    #aoc_puzzle{module = ?MODULE,
                year = 0,
                day = 0,
                name = "not_set",
                expected = {not_set, not_set},
                has_input_file = true}.

-type input_type() :: any().
-type result1_type() :: any().
-type result2_type() :: result1_type().

-spec parse(Input :: binary()) -> input_type().
parse(Input) ->
    Input.

-spec solve1(Input :: input_type()) -> result1_type().
solve1(_Input) ->
    not_implemented.

-spec solve2(Input :: input_type()) -> result2_type().
solve2(_Input) ->
    not_implemented.

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 4
%%% End:
