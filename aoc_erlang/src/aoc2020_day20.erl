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
solve(Input) ->
  Tiles = parse_tiles(Input),
  Size = floor(math:sqrt(maps:size(Tiles))),
  _Coords = lists:sort([{X, Y} ||
                         X <- lists:seq(0, Size - 1),
                         Y <- lists:seq(0, Size - 1)]),

  %% Maps border ids to tile nums
  BorderMap = border_map(Tiles),

  %% Maps tile nums to their border ids
  TilesToBorders = inv_map(BorderMap),

  %% Get the corners
  Corners = find_corner_tiles(TilesToBorders, BorderMap),

  %%Part1Sol = maps:fold(fun(TileNum, _TileData, Acc) -> Acc * TileNum end, 1, Corners),
  %%?debugFmt("Part 1 solution: ~p", [Part1Sol]),

  TopLeft = something,
  ExternalId = something, %% one of external border ids of TopLeft

  %% Rotate TopLeft tile such that the given external id faces left.
  TopLeft0 = rotate_or_flip_to_fit(TopLeft, ExternalId, left),

  %% Place this tile at {0, 0}.
  PlacedTiles = place_tile(TopLeft0, {0, 0}, #{}),

  %%% CONTINUE HERE

  ?debugFmt("Top left tile: ~p", [T]).

%% TODO write a function which can rotate a tile until it has a given
%% border id at a given position, i.e.
%%
%% rotate_tile(Tile, Id, Pos) such that

  %% place_tiles(Coords, Tiles, BorderMap).

%% TODO this map can probably be produced in the same function as
%% border_map
inv_map(Map) ->
  maps:fold(
    fun(K, VList, Acc) ->
        lists:foldl(
          fun(V, InnerAcc) ->
              maps:update_with(V, fun(Old) -> [K|Old] end,
                               [K], InnerAcc)
          end, Acc, VList)
    end, #{}, Map).

border_map(Tiles) ->
  maps:fold(
    fun(TileNum, TileMap, Acc) ->
        Borders = borders(TileMap),
        lists:foldl(
          fun(BorderId, InnerAcc) ->
              %% ?debugFmt("Border ID ~p refers to tilenum ~p", [BorderId, TileNum]),
              maps:update_with(BorderId,
                               fun(Old) -> [TileNum|Old] end,
                               [TileNum], InnerAcc)
          end, Acc, Borders)
    end, #{}, Tiles).

%% This is a bit hairy...
find_corner_tiles(TilesToBorders, BorderMap) ->
  maps:filter(
    fun(TileNum, BorderIds) ->
        ExternalIds =
          lists:filter(
            fun(Id) ->
                %% External ids refer only to *one*
                %% tile in the border-id -> tilenum map.
                [TileNum] =:= maps:get(Id, BorderMap)
            end, BorderIds),
        length(ExternalIds) == 4
    end, TilesToBorders).

%% find_corner_tile([TileNum|Rest], Tiles, BorderMap) ->
%%   Tile = maps:get(TileNum, Tiles),
%%   OuterBorders = [B || B <- borders(Tile), not maps:is_key(B, BorderMap)],
%%   ?debugFmt("Outer borders: ~p", [OuterBorders]),

%%   %% Tiles which have only 2 "inner" borders are corner tiles.
%%   case length(OuterBorders) of
%%     N when N == 2 -> TileNum;
%%     N when N >= 3 -> find_corner_tile(Rest, Tiles, BorderMap)
%%   end.

%% place_tiles(_Coords, Tiles, BorderMap) ->

%%   Corner = find_corner_tile(maps:keys(Tiles), Tiles, BorderMap),
%%   ?debugFmt("Corner tile: ~p~n~s~n",
%%             [Corner, grid:to_str(maps:get(Corner, Tiles))]),

%%   lists:foldl(
%%     fun(Coord, {PlacedTiles, RemainingTiles}) ->
%%         place_tile_at(Coord, PlacedTiles, RemainingTiles, BorderMap)
%%     end, {#{}, Tiles}, []).






%% place_tile_at(Coord, PlacedTiles, RemainingTiles, BorderMap) ->
%%   %% Find tile from `RemainingTiles' to place at `Coord'.
%%   RemainingTileNums = maps:keys(RemainingTiles),
%%   TileToPlace = find_tile(Coord, PlacedTiles, RemainingTileNums, RemainingTiles, BorderMap),
%%   ?debugFmt("Found tile to place at ~p~n~p~n",
%%             [Coord, TileToPlace]),
%%   ok.

%% %% Iterate over all tiles to find a tile to place at `Coord'
%% find_tile(Coord, PlacedTiles, [TileNum|Rest], TileMap, BorderMap) ->
%%   %% Try to place `TileNum' at `Coord'
%%   Tile = maps:get(TileNum, TileMap),
%%   Rotated = flip_rotate_tile(Tile),

%%   case find_tile_rotation(Coord, PlacedTiles, Rotated, TileNum, TileMap, BorderMap) of
%%     false ->
%%       find_tile(Coord, PlacedTiles, Rest, TileMap, BorderMap);
%%     Tile ->
%%       Tile
%%   end.

%% find_tile_rotation(_, _, [], _, _, _) ->
%%   false;
%% find_tile_rotation(Coord, PlacedTiles, [Tile|Rest], TileNum, TileMap, BorderMap) ->
%%   case fits(Coord, PlacedTiles, Tile, TileNum, TileMap, BorderMap) of
%%     true -> Tile;
%%     false ->
%%       find_tile_rotation(Coord, PlacedTiles, Rest, TileNum, TileMap, BorderMap)
%%   end.

%% %% Return true/false if `Tile' fits at `Coord'.
%% fits({_X, _Y}, _PlacedTiles, _Tile, _TileNum, _TileMap, _BorderMap) ->
%%   %% ?debugFmt("Checking if tile ~p fits at ~p",
%%   %%           [TileNum, _Coord]),
%%   false.

shift_by_coord(Coord, Tile, N) ->
  case maps:get(Coord, Tile, undefined) of
    $# -> (N bsl 1) bor 1;
    _ -> N bsl 1
  end.

%% Take the four borders of this tile and translate them to integers
%% for easy indexing. Each border can be flipped, so this makes 8
%% integers per tile.
borders(Tile) ->
  %% TODO store this in the TileData?
  Size = maps:get(size, Tile),
  L = lists:seq(0, Size - 1),
  LR = lists:seq(Size -1, 0, -1),

  N  = lists:foldl(fun(X, Acc) -> shift_by_coord({X, 0},        Tile, Acc) end, 0, L),
  NR = lists:foldl(fun(X, Acc) -> shift_by_coord({X, 0},        Tile, Acc) end, 0, LR),
  S  = lists:foldl(fun(X, Acc) -> shift_by_coord({X, Size - 1}, Tile, Acc) end, 0, L),
  SR = lists:foldl(fun(X, Acc) -> shift_by_coord({X, Size - 1}, Tile, Acc) end, 0, LR),
  E  = lists:foldl(fun(Y, Acc) -> shift_by_coord({Size - 1, Y}, Tile, Acc) end, 0, L),
  ER = lists:foldl(fun(Y, Acc) -> shift_by_coord({Size - 1, Y}, Tile, Acc) end, 0, LR),
  W  = lists:foldl(fun(Y, Acc) -> shift_by_coord({0, Y},        Tile, Acc) end, 0, L),
  WR = lists:foldl(fun(Y, Acc) -> shift_by_coord({0, Y},        Tile, Acc) end, 0, LR),

  [N, NR, S, SR, E, ER, W, WR].

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
  {TileNum, maps:put(size, Width, Map)}.

%% ======================================================================
%% Helpers
%% ======================================================================

flip_rotate_tile(Rows) ->
  TileSize = maps:get(size, Rows),
  Max = TileSize - 1,

  %% ?debugFmt("Flipping/rotating tile (max = ~p):~n~s~n", [Max, grid:to_str(Rows)]),

  Rotate =
    fun(R) ->
        maps:fold(fun({X, Y}, Value, Acc) when (X >= 0) andalso (X =< Max) andalso
                                               (Y >= 0) andalso (Y =< Max) ->
                      maps:put({Max - Y, X}, Value, Acc);
                     (K, V, Acc) -> maps:put(K, V, Acc)
                  end, #{}, R)
    end,

  Flip =
    fun(R) ->
        maps:fold(fun({X, Y}, Value, Acc) when (X >= 0) andalso (X =< Max) andalso
                                               (Y >= 0) andalso (Y =< Max) ->
                      maps:put({Max - X, Y}, Value, Acc);
                     (K, V, Acc) -> maps:put(K, V, Acc)
                  end, #{}, R)
    end,

  R90 = Rotate(Rows),
  R180 = Rotate(R90),
  R270 = Rotate(R180),
  FlipR0 = Flip(Rows),
  FlipR90 = Rotate(FlipR0),
  FlipR180 = Rotate(FlipR90),
  FlipR270 = Rotate(FlipR180),

  %% Self-test
  ?assertEqual(Rows, Rotate(R270)),
  ?assertEqual(Rows, Rotate(Rotate(Rotate(Rotate(Rows))))),

  [Rows, R90, R180, R270, FlipR0, FlipR90, FlipR180, FlipR270].

%% ======================================================================
%% Input
%% ======================================================================

%% get_input() ->
%%   inputs:get_as_binary(2020, 20).

%% ======================================================================
%% Tests
%% ======================================================================

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
  ?_assertEqual(0, solve(test_input())).


%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
