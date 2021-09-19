%% Behavior module for AoC puzzles

-module(aoc_puzzle).

-include("aoc_puzzle.hrl").

-export([info/1, parse/2, solve/2, solve1/2, solve2/2]).

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
