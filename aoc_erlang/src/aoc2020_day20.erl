%%% Advent of Code solution for 2020 day 20.
%%% Created: 2020-12-20T06:51:18+00:00

-module(aoc2020_day20).
-include_lib("eunit/include/eunit.hrl").

-compile([nowarn_unused_function]).

%% Day 20: Jurassic Jigsaw

solve(Input) ->
  Tiles = parse_tiles(Input),
  Size = floor(math:sqrt(maps:size(Tiles) div 8)),
  [First|_Rest] = [{X, Y} || X <- lists:seq(0, Size - 1),
                            Y <- lists:seq(0, Size - 1)],

  %%?debugFmt("~nTiles:~n~p", [Tiles]),

  %% ?debugFmt("~nAll borders:~n~p", [AllBorders]).

  BorderMap = border_map(Tiles),
  %%?debugFmt("~nBorderMap:~n~p", [BorderMap]),

  InvBorderMap = inv_border_map(Tiles),
  %%?debugFmt("~nInverse BorderMap:~n~p", [InvBorderMap]),

  [StartTile] = find_ne_corner_tile(Tiles, BorderMap, InvBorderMap),

  %% Place the first tile
  PlacedTiles = #{First => StartTile},
  RemainingTiles = remove_tile(StartTile, Tiles),

  %% ?debugFmt("Start tile: ~p", [StartTile]),
  %% ?debugFmt("~n~s~n", [grid:to_str(maps:get(StartTile, Tiles))]),

  place_row(0, Size, StartTile, RemainingTiles, BorderMap, InvBorderMap, PlacedTiles).

remove_tile({Num, _}, Tiles) ->
  %% ?debugFmt("Removing all tiles with num ~p", [Num]),
  maps:filter(fun({TileNum, _}, _) when Num =/= TileNum ->
                  true;
                 (_Id, _) ->
                  %% ?debugFmt("Removing ~p", [Id]),
                  false
              end, Tiles).

%% Place row Y
place_row(Y, Size, LeftTile, RemainingTiles, BorderMap, InvBorderMap, PlacedTiles) ->
  {_,
   PlacedTilesOut0,
   RemainingTilesOut0} =
    lists:foldl(
      fun(X, {LeftTileIn, PlacedTilesIn, RemainingTilesIn}) ->
          [_N, _S, E, _W] = maps:get(LeftTileIn, BorderMap),
          Coord = {X, Y},
          {LeftNum, _} = LeftTileIn,
          ?debugFmt("Placing tile right of ~p at coord ~p", [LeftTileIn, Coord]),
          %% ?debugFmt("Already placed tiles ~p", [PlacedTilesIn]),
          ?debugFmt("Joining border id ~p", [E]),

          %% ?debugFmt("~nRemaining tiles ~p", [maps:keys(RemainingTilesIn)]),

          %% There should be 8 different tiles to choose from, we need
          %% the one which where E(Left) == W(Right)

          %% TODO maybe we need a "tile num -> [list of eight tile id]"
          %% map to speed up things.

          [{RightId, _RightData}] =
            maps:to_list(
              maps:filter(
                fun({Num, _} = TileId, _TD) when Num =/= LeftNum ->
                    case maps:get(TileId, BorderMap) of
                      [_, _, _, RightW] when RightW == E -> true;
                      _ -> false
                    end;
                   (_, _) -> false
                end, RemainingTilesIn)),

          ?debugFmt("Placing tile ~p at ~p", [RightId, Coord]),
          %% ?debugFmt("Tile:~n~s~n", [grid:to_str(RightData)]),
          PlacedTilesOut = maps:put(Coord, RightId, PlacedTilesIn),

          %% Remove all symmetries of the tile we just placed
          RemainingTilesOut = remove_tile(RightId, RemainingTilesIn),

          LeftTileOut = RightId,

          {LeftTileOut, PlacedTilesOut, RemainingTilesOut}
      end, {LeftTile, PlacedTiles, RemainingTiles}, lists:seq(1, Size - 1)),

  if Y + 1 == Size ->
      %% No more rows to place
      PlacedTilesOut0;
     true ->
      %% ?debugFmt("Placed tiles: ~p", [PlacedTilesOut0]),
      NewLeftTile = find_tile_below(LeftTile, RemainingTilesOut0, BorderMap),
      %% ?debugFmt("New left tile: ~p", [NewLeftTile]),
      Coord0 = {0, Y + 1},

      PlacedTilesOut1 = maps:put(Coord0, NewLeftTile, PlacedTilesOut0),
      RemainingTilesOut1 = remove_tile(NewLeftTile, RemainingTilesOut0),

      place_row(Y + 1, Size, NewLeftTile, RemainingTilesOut1, BorderMap, InvBorderMap, PlacedTilesOut1)
  end.

find_tile_below(Tile, RemainingTiles, BorderMap) ->
  [_, S, _, _] = maps:get(Tile, BorderMap),

  %% ?debugFmt("Remaining tiles: ~p", [maps:keys(RemainingTiles)]),
  %% ?debugFmt("Finding tile with north edge == ~p", [S]),
  %% ?debugFmt("Borders of remaining tiles: ~p",
  %%           [lists:foldl(fun(Id, Acc) ->
  %%                            maps:put(Id, maps:get(Id, BorderMap), Acc)
  %%                        end, #{}, maps:keys(RemainingTiles))]),
  [{BelowId, _}] =
    maps:to_list(
      maps:filter(
        fun({_Num, _} = TileId, _TD) ->
            [BelowN|_] = maps:get(TileId, BorderMap),
            BelowN == S
        end, RemainingTiles)),
  BelowId.


find_ne_corner_tile(Tiles, BorderMap, InvBorderMap) ->
  maps:fold(
    fun({_, r0} = TileId, _TileData, Acc) ->
        Borders = maps:get(TileId, BorderMap),
        case lists:map(
               fun(BorderId) ->
                   case is_external_border(BorderId, InvBorderMap) of
                     true -> external;
                     false -> BorderId
                   end
               end, Borders) of
          %% Start with NW (top left) tile
          %% Order is N S E W
          [external, _S, _E, external] ->
            [TileId|Acc];
          _ ->
            Acc
        end;
       (_, _, Acc) ->
        Acc
    end, [], Tiles).


%% External borders are borders which only belong to one tile.
is_external_border(BorderId, InvBorderMap) ->
  length(maps:get(BorderId, InvBorderMap)) == 1.

%% Inverse border map; maps border ids to their tile numbers
inv_border_map(Tiles) ->
  maps:fold(
    fun({TileNum, _Sym}, Data, Acc) ->
        Borders = borders(Data),
        lists:foldl(
          fun(Border, InnerAcc) ->
              maps:update_with(
                Border,
                fun(Old) -> lists:usort([TileNum|Old]) end,
                [TileNum], InnerAcc)
          end, Acc, Borders)
    end, #{}, Tiles).

%% Return a map of tile ids to their possible border ids
border_map(Tiles) ->
  maps:fold(fun(TileId, TileData, Acc) ->
                maps:put(TileId, borders(TileData), Acc)
            end, #{}, Tiles).

%% ======================================================================
%% Parser
%% ======================================================================

parse_tiles(Input) ->
  L = binary:split(Input, <<"Tile ">>, [global]),
  lists:foldl(fun(<<>>, Acc) -> Acc;
                 (TileBin, Acc) ->
                  maps:merge(Acc, parse_tile(TileBin))
              end, #{}, L).

parse_tile(TileBin) ->
  [Header, Rows] = binary:split(TileBin, <<"\n">>),
  {match, Matches} =
    re:run(Header, "(\\d+):", [{capture, all_but_first, list}]),
  TileNum = list_to_integer(hd(Matches)),
  [{Width, _}|_] = binary:matches(Rows, <<"\n">>),
  Offsets = binary:matches(Rows, <<"#">>),
  TileData =
    lists:foldl(fun({Offset, _}, Acc) ->
                    maps:put({Offset rem (Width + 1),
                              Offset div (Width + 1)}, $#, Acc)
                end, #{}, Offsets),

  all_symmetries(TileNum, maps:put(size, Width, TileData)).

shift_by_coord(Coord, Tile, N) ->
  case maps:get(Coord, Tile, undefined) of
    $# -> (N bsl 1) bor 1;
    _ -> N bsl 1
  end.

borders(Tile) ->
  Size = maps:get(size, Tile),
  L = lists:seq(0, Size - 1),
  N = lists:foldl(fun(X, Acc) -> shift_by_coord({X, 0},        Tile, Acc) end, 0, L),
  S = lists:foldl(fun(X, Acc) -> shift_by_coord({X, Size - 1}, Tile, Acc) end, 0, L),
  E = lists:foldl(fun(Y, Acc) -> shift_by_coord({Size - 1, Y}, Tile, Acc) end, 0, L),
  W = lists:foldl(fun(Y, Acc) -> shift_by_coord({0, Y},        Tile, Acc) end, 0, L),
  [N, S, E, W].


%% find_external_borders(Map, AllBorders) ->
%%   maps:fold(
%%     fun({TileNum, r0}, Data, Acc) ->
%%         Borders = maps:get(borders, Data),      % borders of this tile
%%         ExternalBorders =
%%           lists:foldl(
%%             fun(Border, InnerAcc) ->
%%                 L = lists:filter(fun(B) ->
%%                                      Border == B
%%                                  end, AllBorders),

%%                 ?debugFmt("matching borders for ~p: ~p", [Border, L]),

%%                 IsExternal = (length(L) == 1),
%%                 case IsExternal of
%%                   true -> [Border|InnerAcc];
%%                   false -> InnerAcc
%%                 end
%%             end, [], Borders),
%%         maps:put(TileNum, ExternalBorders, Acc);
%%        (K, V, Acc) ->
%%         maps:put(K, V, Acc)
%%     end, #{}, Map).


%% ======================================================================
%% Helpers
%% ======================================================================

all_symmetries(Num, Rows) ->
  TileSize = maps:get(size, Rows),
  Max = TileSize - 1,

  Rotate =
    fun(R) ->
        maps:fold(fun({X, Y}, Value, Acc) when (X >= 0) andalso (X =< Max) andalso
                                               (Y >= 0) andalso (Y =< Max) ->
                      maps:put({Max - Y, X}, Value, Acc);
                     (K, V, Acc) ->
                      maps:put(K, V, Acc)
                  end, #{}, R)
    end,

  Flip =
    fun(R) ->
        maps:fold(fun({X, Y}, Value, Acc) when (X >= 0) andalso (X =< Max) andalso
                                               (Y >= 0) andalso (Y =< Max) ->
                      maps:put({Max - X, Y}, Value, Acc);
                     (K, V, Acc) ->
                      maps:put(K, V, Acc)
                  end, #{}, R)
    end,

  R90      = Rotate(Rows),
  R180     = Rotate(R90),
  R270     = Rotate(R180),
  FlipR0   = Flip(Rows),
  FlipR90  = Rotate(FlipR0),
  FlipR180 = Rotate(FlipR90),
  FlipR270 = Rotate(FlipR180),

  %% Self-test
  ?assertEqual(Rows, Rotate(R270)),
  ?assertEqual(Rows, Rotate(Rotate(Rotate(Rotate(Rows))))),

  #{{Num, 'r0'}   => Rows,
    {Num, 'r90'}  => R90,
    {Num, 'r180'} => R180,
    {Num, 'r270'} => R270,
    {Num, 'f0'}   => FlipR0,
    {Num, 'f90'}  => FlipR90,
    {Num, 'f180'} => FlipR180,
    {Num, 'f270'} => FlipR270}.

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
