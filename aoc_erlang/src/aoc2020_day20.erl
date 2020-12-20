%%% Advent of Code solution for 2020 day 20.
%%% Created: 2020-12-20T06:51:18+00:00

-module(aoc2020_day20).
-include_lib("eunit/include/eunit.hrl").

-compile([nowarn_unused_function]).

%% Day 20: Jurassic Jigsaw

%% Part 1: find the corner tiles and multiply their id.  We cheat a
%% little by picking out the 4 tiles which have two borders which do
%% not match up with any other border, but we don't actually compute
%% how any of the tiles are supposed to be flipped/rotated.
part1(Input) ->
  Tiles = parse_tiles(Input),
  CornerTiles = find_corner_tiles(Tiles),
  lists:foldl(fun erlang:'*'/2, 1, CornerTiles).

%% Part 2: Assemble the entire picture and find the sea monster.
part2(Input) ->
  TilesFull = parse_tiles2(Input),
  TilesFull.
  %% [Num|_] = maps:keys(TilesFull),
  %% Rows = maps:get(Num, TilesFull),
  %% flip_rotate_tile(Rows).


%% ======================================================================
%% Parser
%% ======================================================================

parse_tiles(Input) ->
  L = binary:split(Input, <<"Tile ">>, [global]),
  lists:foldl(fun(<<>>, Acc) -> Acc;
                 (TileBin, Acc) ->
                  {Num, Rows} = parse_tile(TileBin),
                  maps:put(Num, Rows, Acc)
              end, #{}, L).

parse_tile(TileBin) ->
  [Header|Rows] = binary:split(TileBin, <<"\n">>, [global]),
  {match, Matches} =
    re:run(Header, "(\\d+):", [{capture, all_but_first, list}]),
  TileNum = list_to_integer(hd(Matches)),
  Rows0 = lists:filtermap(fun(<<>>) -> false;
                             (Binary) -> {true, binary_to_list(Binary)}
                          end, Rows),
  {TileNum, borders(Rows0)}.

%% Parse full tiles (not just their borders)
parse_tiles2(Input) ->
  L = binary:split(Input, <<"Tile ">>, [global]),
  lists:foldl(fun(<<>>, Acc) -> Acc;
                 (TileBin, Acc) ->
                  {Num, Rows} = parse_tile2(TileBin),
                  maps:put(Num, Rows, Acc)
              end, #{}, L).

parse_tile2(TileBin) ->
  [Header, Rows] = binary:split(TileBin, <<"\n">>),
  {match, Matches} =
    re:run(Header, "(\\d+):", [{capture, all_but_first, list}]),
  TileNum = list_to_integer(hd(Matches)),
  [{Width, _}|_] = binary:matches(Rows, <<"\n">>),
  Offsets = binary:matches(Rows, <<"#">>),
  Map =
    lists:foldl(fun({Offset, _}, Acc) ->
                    maps:put({Offset rem (Width + 1),
                              Offset div (Width + 1)}, $#, Acc)
                end, #{}, Offsets),
  {TileNum, Map}.

%% ======================================================================
%% Helpers
%% ======================================================================

%% Return the borders of a tile
borders(Rows) ->
  N = hd(Rows),
  S = lists:last(Rows),
  {W, E} = lists:foldl(
             fun([], Acc) -> Acc;
                ([W|Rest], {W0, E0}) ->
                 E = lists:last(Rest),
                 {[W|W0], [E|E0]}
             end, {[], []}, Rows),
  Rev = fun(List) -> lists:reverse(List) end,
  {N, Rev(E), S, Rev(W)}.

%% Returns true/false if two tiles have a matching border
has_matching_borders(RowsA, RowsB) ->
  MatchingBorders =
    [{A, B} || A <- tuple_to_list(RowsA),
               B <- tuple_to_list(RowsB),
               is_matching_borders(A, B)],
  length(MatchingBorders) > 0.

%% Return true/false if two borders match (possibly with flipping)
is_matching_borders(RowA, RowB) ->
  (RowA =:= RowB) orelse (RowA =:= lists:reverse(RowB)).

%% Find the id of the corner tiles
find_corner_tiles(Tiles) ->
  TileNums = maps:keys(Tiles),
  lists:foldl(
    fun(Num, Acc) ->
        Borders = maps:get(Num, Tiles),
        MatchingTiles =
          lists:filter(
            fun(Other) when Other =/= Num ->
                has_matching_borders(Borders, maps:get(Other, Tiles));
               (_) -> false
            end, TileNums),

        case length(MatchingTiles) of
          2 -> [Num|Acc];
          N when N >= 3 -> Acc
        end
    end, [], TileNums).

%% There are 8 ways to rotate/flip a tile
%% 0: Start
%% 1: Rotate 90 CW (or 270 CCW)
%% 2: Rotate 180 (CW/CCW)
%% 3: Rotate 270 CW (or 90 CCW)
%% 4: Flip around vertical axis
%% 5: Flip around horizontal axis
%% 6: Flip around vertical axis + rotate 90 CW
%% 7: Flip around horizontal axis + rotate 90 CCW

flip_rotate_tile(Rows) ->
  [Rows,
   rotate_90cw(Rows),
   rotate_180cw(Rows),
   rotate_90ccw(Rows),
   flip_vert(Rows),
   flip_horiz(Rows),
   flip_vert_90cw(Rows),
   flip_horiz_90cw(Rows)].

rotate_90cw(Rows) ->
  Rows.

rotate_180cw(Rows) ->
  Rows.

rotate_90ccw(Rows) ->
  Rows.

flip_vert(Rows) ->
  Rows.

flip_horiz(Rows) ->
  lists:reverse(Rows).

flip_vert_90cw(Rows) ->
  Rows.

flip_horiz_90cw(Rows) ->
  _FlipHoriz = lists:reverse(Rows).



%% ======================================================================
%% Input
%% ======================================================================

get_input() ->
  inputs:get_as_binary(2020, 20).

%% ======================================================================
%% Tests
%% ======================================================================

main_test_() ->
  Input = get_input(),
  [ {"Part 1", ?_assertEqual(66020135789767, part1(Input))}
  , {"Part 2", ?_assertEqual(0, part2(Input))}
  ].

%% Check that all borders are pairwise unique, i.e. each border
%% matches up with exactly 1 (or zero, for external tile borders)
%% other tile.
check_border_uniqueness_test_() ->
  Tiles = parse_tiles(get_input()),
  AllBorders =
    lists:foldl(fun(Borders, Acc) ->
                    tuple_to_list(Borders) ++ Acc
                end, [], maps:values(Tiles)),

  UniqueBorderPairs =
    lists:sort([A || A <- AllBorders,
                     B <- AllBorders,
                     A < B,
                     is_matching_borders(A, B)]),

  ?_assertEqual(UniqueBorderPairs, lists:usort(UniqueBorderPairs)).

borders_test_() ->
  Rows = ["####",
          "....",
          "....",
          "###."],
  ?_assertEqual({"####",
                 "#...",
                 "###.",
                 "#..#"}, borders(Rows)).

is_matching_border_test_() ->
  [ ?_assert(is_matching_borders("##.##.", ".##.##"))
  , ?_assertNot(is_matching_borders("##.##.", ".##.#."))
  ].

matching_borders_test_() ->
  %% These two have a matching south border
  RowsA = [".###.",
           "....#",
           "....#",
           ".....",
           "###.."],

  RowsB = [".#.#.",
           "#....",
           ".....",
           "....#",
           "..###"],

  BordersA = borders(RowsA),
  BordersB = borders(RowsB),

  ?_assert(has_matching_borders(BordersA, BordersB)).

test_input() ->
  <<"Tile 2311:\n"
    "..##.#..#.\n"
    "##..#.....\n"
    "#...##..#.\n"
    "####.#...#\n"
    "##.##.###.\n"
    "##...#.###\n"
    ".#.#.#..##\n"
    "..#....#..\n"
    "###...#.#.\n"
    "..###..###\n"
    "\n"
    "Tile 1951:\n"
    "#.##...##.\n"
    "#.####...#\n"
    ".....#..##\n"
    "#...######\n"
    ".##.#....#\n"
    ".###.#####\n"
    "###.##.##.\n"
    ".###....#.\n"
    "..#.#..#.#\n"
    "#...##.#..\n"
    "\n"
    "Tile 1171:\n"
    "####...##.\n"
    "#..##.#..#\n"
    "##.#..#.#.\n"
    ".###.####.\n"
    "..###.####\n"
    ".##....##.\n"
    ".#...####.\n"
    "#.##.####.\n"
    "####..#...\n"
    ".....##...\n"
    "\n"
    "Tile 1427:\n"
    "###.##.#..\n"
    ".#..#.##..\n"
    ".#.##.#..#\n"
    "#.#.#.##.#\n"
    "....#...##\n"
    "...##..##.\n"
    "...#.#####\n"
    ".#.####.#.\n"
    "..#..###.#\n"
    "..##.#..#.\n"
    "\n"
    "Tile 1489:\n"
    "##.#.#....\n"
    "..##...#..\n"
    ".##..##...\n"
    "..#...#...\n"
    "#####...#.\n"
    "#..#.#.#.#\n"
    "...#.#.#..\n"
    "##.#...##.\n"
    "..##.##.##\n"
    "###.##.#..\n"
    "\n"
    "Tile 2473:\n"
    "#....####.\n"
    "#..#.##...\n"
    "#.##..#...\n"
    "######.#.#\n"
    ".#...#.#.#\n"
    ".#########\n"
    ".###.#..#.\n"
    "########.#\n"
    "##...##.#.\n"
    "..###.#.#.\n"
    "\n"
    "Tile 2971:\n"
    "..#.#....#\n"
    "#...###...\n"
    "#.#.###...\n"
    "##.##..#..\n"
    ".#####..##\n"
    ".#..####.#\n"
    "#..#.#..#.\n"
    "..####.###\n"
    "..#.#.###.\n"
    "...#.#.#.#\n"
    "\n"
    "Tile 2729:\n"
    "...#.#.#.#\n"
    "####.#....\n"
    "..#.#.....\n"
    "....#..#.#\n"
    ".##..##.#.\n"
    ".#.####...\n"
    "####.#.#..\n"
    "##.####...\n"
    "##..#.##..\n"
    "#.##...##.\n"
    "\n"
    "Tile 3079:\n"
    "#.#.#####.\n"
    ".#..######\n"
    "..#.......\n"
    "######....\n"
    "####.#..#.\n"
    ".#...#.##.\n"
    "#.#####.##\n"
    "..#.###...\n"
    "..#.......\n"
    "..#.###...\n">>.

ex1_test_() ->
  ?_assertEqual(20899048083289, part1(test_input())).


%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
