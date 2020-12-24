%%% Advent of Code solution for 2020 day 24.
%%% Created: 2020-12-24T05:02:46+00:00

-module(aoc2020_day24).
-include_lib("eunit/include/eunit.hrl").

%% For an explanation of how to use 3D coords to represent hexagons,
%% https://www.redblobgames.com/grids/hexagons/

%% ======================================================================
%% Part 1
%% ======================================================================
part1(Input) ->
  Tiles = flip_tiles(Input),
  maps:size(Tiles).

%% ======================================================================
%% Part 2
%% ======================================================================
part2(Input, N) ->
  Tiles = flip_tiles(Input),
  Final = lists:foldl(fun do_one_iter/2, Tiles, lists:seq(1, N)),
  maps:size(Final).

%% ======================================================================
%% Solutions
%% ======================================================================

%% Perform the initial tile flipping.
flip_tiles(Input) ->
  lists:foldl(
    fun(Line, Acc) ->
        FinalCoord =
          fold_coords(
            fun(Dir, {X, Y, Z}) ->
                %% Follow the conventions from
                %% https://www.redblobgames.com/grids/hexagons/#neighbors
                %% Note: sum of all the +1/-1 must be zero.
                case Dir of
                  "ne" -> {X + 1, Y,     Z - 1};
                  "e"  -> {X + 1, Y - 1, Z    };
                  "se" -> {X,     Y - 1, Z + 1};
                  "sw" -> {X - 1, Y,     Z + 1};
                  "w"  -> {X - 1, Y + 1, Z    };
                  "nw" -> {X,     Y + 1, Z - 1}
                end
            end, {0, 0, 0}, Line),

        %% Only store black keys, all other tiles are white
        case maps:is_key(FinalCoord, Acc) of
          true -> maps:remove(FinalCoord, Acc);
          false -> maps:put(FinalCoord, black, Acc)
        end

    end, #{}, Input).

fold_coords(_Fun, State, []) ->
  State;
fold_coords(Fun, State, [A, B|Rest]) when ([A, B] =:= "ne") orelse
                                          ([A, B] =:= "nw") orelse
                                          ([A, B] =:= "sw") orelse
                                          ([A, B] =:= "se") ->
  fold_coords(Fun, Fun([A, B], State), Rest);
fold_coords(Fun, State, [A|Rest]) when ([A] =:= "e") orelse
                                       ([A] =:= "w") ->
  fold_coords(Fun, Fun([A], State), Rest).

%% ======================================================================
%% Part 2 iteration code
%% ======================================================================

do_one_iter(_N, Tiles) ->
  %% Only consider black tiles and their neighbors (white tiles can
  %% only turn black if they have a black neighbor)
  Coords =
    sets:to_list(
      maps:fold(
        fun(Coord, _, Acc) ->
            sets:union(Acc, sets:from_list([Coord|neighbors(Coord)]))
        end, sets:new(), Tiles)),

  lists:foldl(
    fun(Coord, Acc) ->
        Neighbors = neighbors(Coord),
        IsBlack = maps:is_key(Coord, Tiles),
        case {IsBlack, count_black_neighbors(Neighbors, Tiles)} of
          {true, 0} -> maps:remove(Coord, Acc);
          {true, NB} when NB > 2 -> maps:remove(Coord, Acc);
          {false, 2} -> maps:put(Coord, black, Acc);
          _ -> Acc
        end
    end, Tiles, Coords).

count_black_neighbors(Neighbors, Tiles) ->
  lists:foldl(
    fun(Coord, N) ->
        case maps:get(Coord, Tiles, white) of
          black -> N + 1;
          _ -> N
        end
    end, 0, Neighbors).

neighbors({X, Y, Z} = Coord) ->
  %% Cache neighbor lists in the process dictionary (these are static;
  %% the neighbors of a given tile will always be the same).
  case get(Coord) of
    undefined ->
      Nbrs = [{X + Dx, Y + Dy, Z + Dz} ||
               Dx <- [-1, 0, 1],
               Dy <- [-1, 0, 1],
               Dz <- [-1, 0, 1],
               {Dy, Dx, Dz} =/= {0, 0, 0},
               Dy + Dx + Dz == 0],
      put(Coord, Nbrs),
      Nbrs;
    Nbrs -> Nbrs
  end.

%% Input reader (place downloaded input file in
%% priv/inputs/2020/input24.txt).
get_input() ->
  inputs:get_as_lines(2020, 24).

%% Tests
main_test_() ->
  Input = get_input(),

  [ {"Part 1", ?_assertEqual(300, part1(Input))}
  , {"Part 2", ?_assertEqual(3466, part2(Input, 100))}
  ].

neighbors_test_() ->
  ?_assertEqual([{-1,0,1},
                 {-1,1,0},
                 {0,-1,1},
                 {0,1,-1},
                 {1,-1,0},
                 {1,0,-1}], lists:sort(neighbors({0, 0, 0}))).

fold_coords_test_() ->
  ?_assertEqual(["e", "se", "ne", "e"],
                fold_coords(fun(Dir, Acc) ->
                                Acc ++ [Dir]
                            end, [], "esenee")).

test_input() ->
  [ "sesenwnenenewseeswwswswwnenewsewsw"
  , "neeenesenwnwwswnenewnwwsewnenwseswesw"
  , "seswneswswsenwwnwse"
  , "nwnwneseeswswnenewneswwnewseswneseene"
  , "swweswneswnenwsewnwneneseenw"
  , "eesenwseswswnenwswnwnwsewwnwsene"
  , "sewnenenenesenwsewnenwwwse"
  , "wenwwweseeeweswwwnwwe"
  , "wsweesenenewnwwnwsenewsenwwsesesenwne"
  , "neeswseenwwswnwswswnw"
  , "nenwswwsewswnenenewsenwsenwnesesenew"
  , "enewnwewneswsewnwswenweswnenwsenwsw"
  , "sweneswneswneneenwnewenewwneswswnese"
  , "swwesenesewenwneswnwwneseswwne"
  , "enesenwswwswneneswsenwnewswseenwsese"
  , "wnwnesenesenenwwnenwsewesewsesesew"
  , "nenewswnwewswnenesenwnesewesw"
  , "eneswnwswnwsenenwnwnwwseeswneewsenese"
  , "neswnwewnwnwseenwseesewsenwsweewe"
  , "wseweeenwnesenwwwswnew"
  ].

ex1_test_() ->
  ?_assertEqual(23, part2(test_input(), 5)).

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
