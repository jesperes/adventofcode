%%% Advent of Code solution for 2020 day 11.
%%% Created: 2020-12-11T06:57:57+00:00

-module(aoc2020_day11).
-include_lib("eunit/include/eunit.hrl").

%% Puzzle solution
part1(Lines) ->
  Grid = to_map(Lines),
  iterate_until_same(Grid, fun compute_next1/3).

part2(Lines) ->
  Grid = to_map(Lines),
  iterate_until_same(Grid, fun compute_next2/3).
  %% G0 = iterate_one_step(Grid, fun compute_next2/3),
  %% G1 = iterate_one_step(G0, fun compute_next2/3),
  %% G2 = iterate_one_step(G1, fun compute_next2/3),
  %% ?debugFmt("~n~s~n", [grid:to_str(G2)]).

%% Iterate until the grid does not change
iterate_until_same(Grid, Fun) ->
  Next = iterate_one_step(Grid, Fun),
  case Next =:= Grid of
    true ->
      maps:fold(fun(_, $#, Acc) -> Acc + 1;
                   (_, _, Acc) -> Acc
                end, 0, Next);
    false ->
      iterate_until_same(Next, Fun)
  end.

iterate_one_step(Grid, Fun) ->
  maps:fold(
    fun(K, V, Acc) ->
        maps:put(K, Fun(K, V, Grid), Acc)
    end, #{}, Grid).

%% Compute the next state of cell `V' at coordinate `Coord'.
compute_next1(Coord, V, OldGrid) ->
  OccupiedAdj = occupied_adjacents(Coord, OldGrid),
  case V of
    $L when OccupiedAdj == 0 -> $#;             % become occupied
    $# when OccupiedAdj >= 4 -> $L;             % become free
    _ -> V                                      % unchanged
  end.

occupied_adjacents({X, Y}, Grid) ->
  Deltas = [{-1, -1}, {0, -1}, {1, -1},
            {-1,  0},          {1,  0},
            {-1,  1}, {0, 1},  {1,  1}],

  lists:foldl(
    fun({Dx, Dy}, Acc) ->
        case maps:get({X + Dx, Y + Dy}, Grid, undefined) of
          $# -> Acc + 1;
          _ -> Acc
        end
    end, 0, Deltas).

%% Compute the next state of cell `V' at coordinate `Coord'.
compute_next2(Coord, V, OldGrid) ->
  VisibleAdj = length(visible_adjacents(Coord, OldGrid)),
  case V of
    $L when VisibleAdj == 0 -> $#;             % become occupied
    $# when VisibleAdj >= 5 -> $L;             % become free
    _ -> V                                     % unchanged
  end.

visible_adjacents(Coord, Grid) ->
  Deltas = [{-1, -1}, {0, -1}, {1, -1},
            {-1,  0},          {1,  0},
            {-1,  1}, {0, 1},  {1,  1}],

  lists:foldl(
    fun(Delta, Acc) ->
        case find_first_in_direction(Coord, Delta, 1, Grid) of
          {_, _} = Adj -> [Adj|Acc];
          false -> Acc
        end
    end, [], Deltas).

find_first_in_direction({X, Y} = Coord, {Dx, Dy} = Delta, Dist, Grid) ->
  VisibleCoord = {X + Dx * Dist, Y + Dy * Dist},
  case maps:get(VisibleCoord, Grid, undefined) of
    $# -> VisibleCoord;
    $. -> find_first_in_direction(Coord, Delta, Dist + 1, Grid);
    _ -> false
  end.

%% Parse input lines to a map
to_map(Lines) ->
  {_, Grid} =
    lists:foldl(
      fun(L, {Y, Map}) ->
          {_, MapOut} =
            lists:foldl(
              fun(C, {X, Acc}) ->
                  {X + 1, maps:put({X, Y}, C, Acc)}
              end, {0, Map}, L),
          {Y + 1, MapOut}
      end, {0, #{}}, Lines),
  Grid.

%% Input reader (place downloaded input file in
%% priv/inputs/2020/input11.txt).
get_input() ->
  inputs:get_as_lines(2020, 11).

%% Tests
main_test_() ->
  Input = get_input(),

  [ {"Part 1", ?_assertEqual(2093, part1(Input))}
  , {"Part 2", ?_assertEqual(1862, part2(Input))}
  ].

test_input() ->
  ["L.LL.LL.LL",
   "LLLLLLL.LL",
   "L.L.L..L..",
   "LLLL.LL.LL",
   "L.LL.LL.LL",
   "L.LLLLL.LL",
   "..L.L.....",
   "LLLLLLLLLL",
   "L.LLLLLL.L",
   "L.LLLLL.LL"].

ex1_test_() ->
  ?_assertEqual(37, part1(test_input())).

ex2_test_() ->
  ?_assertEqual(26, part2(test_input())).

test_input2() ->
  [".##.##.",
   "#.#.#.#",
   "##...##",
   "...L...",
   "##...##",
   "#.#.#.#",
   ".##.##."].

ex2b_test_() ->
  Grid = to_map(test_input2()),
  ?_assertEqual([], visible_adjacents({3, 3}, Grid)).

test_input3() ->
  [".......#.",
   "...#.....",
   ".#.......",
   ".........",
   "..#L....#",
   "....#....",
   ".........",
   "#........",
   "...#....."].

ex2c_test_() ->
  Grid = to_map(test_input3()),
  Adj = visible_adjacents({3, 4}, Grid),
  ?_assertEqual(8, length(Adj)).

test_input4() ->
  [".............",
   ".L.L.#.#.#.#.",
   "............."].

ex2d_test_() ->
  Grid = to_map(test_input4()),
  [ ?_assertEqual([], visible_adjacents({1, 1}, Grid)),
    ?_assertEqual([{5, 1}], visible_adjacents({3, 1}, Grid))
  ].

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
