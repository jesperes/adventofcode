%% Behavior module for AoC puzzles

-module(aoc_puzzle).

-include("aoc_puzzle.hrl").

-export([find_puzzles/2, info/1, parse/2, solve/2, solve1/2, solve2/2]).

-optional_callbacks([solve/1, solve1/1, solve2/1]).

%% Callback to parse the input data
-callback parse(Input :: binary()) -> ParsedInput :: term().
%% Callback to part 1 of the puzzle
-callback solve1(ParsedInput :: term()) -> term().
%% Callback to part 2 of the puzzle
-callback solve2(ParsedInput :: term()) -> term().
%% Callback for puzzles where we solve both puzzles in one call
-callback solve(ParsedInput :: term()) -> term().
%% Callback to get info about a puzzle
-callback info() -> aoc_puzzle().

info(M) ->
    M:info().

parse(M, Binary) ->
    M:parse(Binary).

solve(M, Input) ->
    M:solve(Input).

solve1(M, Input) ->
    M:solve1(Input).

solve2(M, Input) ->
    M:solve2(Input).

-spec find_puzzles(Year :: integer() | all, Day :: integer() | all) ->
                      [Puzzles :: aoc_puzzle()].
find_puzzles(Year, Day) ->
    {{ThisYear, _, _}, _} = erlang:localtime(),
    lists:filtermap(fun({M, _Y, _D}) ->
                       try
                           ModInfo = M:module_info(),
                           Attrs = proplists:get_value(attributes, ModInfo),
                           Behaviors = proplists:get_value(behavior, Attrs),
                           case Behaviors =/= undefined andalso lists:member(aoc_puzzle, Behaviors)
                           of
                               true -> {true, M:info()};
                               false -> false
                           end
                       catch
                           error:undef -> false
                       end
                    end,
                    [{list_to_atom(lists:flatten(
                                       io_lib:format("aoc~w_day~2..0w", [Y, D]))),
                      Y,
                      D}
                     || Y
                            <- case Year of
                                   all ->
                                       lists:seq(2015, ThisYear);
                                   Y when is_integer(Y) ->
                                       [Y]
                               end,
                        D
                            <- case Day of
                                   all ->
                                       lists:seq(1, 25);
                                   D when is_integer(D) ->
                                       [D]
                               end]).
