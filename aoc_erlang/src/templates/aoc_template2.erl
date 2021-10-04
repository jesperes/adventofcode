-module(aoc_template2).

-behavior(aoc_puzzle).

-export([parse/1, solve1/1, solve2/1, info/0]).

-include("aoc_puzzle.hrl").

-spec info() -> aoc_puzzle().
info() ->
    #aoc_puzzle{module = ?MODULE,
                year = 0,
                day = 0,
                name = "TBD",
                expected = {0, 0},
                has_input_file = true}.

-type input_type() :: [string()].
-type result_type() :: integer().

-spec parse(Input :: binary()) -> input_type().
parse(Input) ->
    string:tokens(binary_to_list(Input), "\n\r").

-spec solve1(Input :: input_type()) -> result_type().
solve1(_Input) ->
    0.

-spec solve2(Input :: input_type()) -> result_type().
solve2(_Input) ->
    0.

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 4
%%% End:
