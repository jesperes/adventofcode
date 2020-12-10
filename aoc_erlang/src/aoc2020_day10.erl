%%% Advent of Code solution for 2020 day 10.
%%% Created: 2020-12-10T06:46:57+00:00

-module(aoc2020_day10).
-include_lib("eunit/include/eunit.hrl").

%% Puzzle solution
part1(Input) ->
  solve1(Input).

part2(Input) ->
  solve2(Input).

%% ======================================================================
%% Part 1
%% ======================================================================

solve1(List) ->
  Adapters = lists:sort(List),
  #{3 := D3, 1 := D1} = adapt(0, Adapters, #{}),
  D3 * D1.

adapt(_Jolts, [], Acc) ->
  incr(3, Acc); %% include last 3-diff to device
adapt(Jolts, [Next|List], Acc) ->
  Diff = Next - Jolts,
  adapt(Jolts + Diff, List, incr(Diff, Acc)).

incr1(N) -> N + 1.

incr(N, Map) ->
  maps:update_with(N, fun incr1/1, 1, Map).

%% ======================================================================
%% Part 2
%% ======================================================================

solve2(List) ->
  Adapters = lists:sort(List),
  Device = lists:max(Adapters) + 3,

  %% Idea borrowed from
  %% https://www.reddit.com/r/adventofcode/comments/ka8z8x/2020_day_10_solutions/gf990qj/

  %% Cache is a maps from indexes in `Adapters' to the number
  %% of ways it can be reached.
  Cache = #{0 => 1},

  %% For each adapter, add the number of ways the previous three
  %% adapters can be reached.
  Cache0 =
    lists:foldl(fun(I, CacheIn) ->
                    Sum3 = lists:sum([maps:get(I - X, CacheIn, 0) ||
                                       X <- [1, 2, 3], I - X >= 0]),
                    maps:put(I, Sum3, CacheIn)
                end, Cache, Adapters ++ [Device]),

  lists:max(maps:values(Cache0)).

%% Input reader (place downloaded input file in
%% priv/inputs/2020/input10.txt).
get_input() ->
  inputs:get_as_ints(2020, 10).

%% Tests
main_test_() ->
  Input = get_input(),

  [ {"Part 1", ?_assertEqual(2738, part1(Input))}
  , {"Part 2", ?_assertEqual(74049191673856, part2(Input))}
  ].

test_input() -> [16, 10, 15, 5, 1, 11, 7, 19, 6, 12, 4].

ex1_test_() ->
  ?_assertEqual(35, solve1(test_input())).

ex2_test_() ->
  ?_assertEqual(8, solve2(test_input())).

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
