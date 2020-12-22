%%% Advent of Code solution for 2020 day 24.
%%% Created: 2020-12-22T21:10:14+00:00

-module(aoc2020_day24).
-behavior(aoc_puzzle).

-export([info/0,
         solve/1,
         parse/1]).

-include("aoc_puzzle.hrl").

%% ======================================================================
%% Info
%% ======================================================================
-spec info() -> aoc_puzzle().
info() ->
  #aoc_puzzle{module = ?MODULE,
              year = 2020,
              day = 24,
              name = "AoC 2020 day 24"}.

%% ======================================================================
%% Solver
%% ======================================================================
-spec solve(ParsedInput :: term()) -> ok.
solve(_Input) ->
  timer:sleep(1000),
  ok.

%% ======================================================================
%% Parser
%% ======================================================================
-spec parse(Input :: binary()) -> ParsedInput :: term().
parse(Input) ->
  Input.

%% ======================================================================
%% Tests
%% ======================================================================

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").

main_test_() ->
  _Input = aoc_puzzle:get_parsed_input(?MODULE),
  %% ...
  ?_assert(false).

-endif.

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
