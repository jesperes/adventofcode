%%% Advent of Code solution for 2020 day 17.
%%% Created: 2020-12-17T07:17:47+00:00

-module(aoc2020_day17).
-include_lib("eunit/include/eunit.hrl").

-compile([nowarn_unused_function]).

%% Entry points
part1(Input) ->
  Cubes = parse_3d(Input),
  conway_3d(Cubes, 6).

part2(Input) ->
  Cubes = parse_4d(Input),
  conway_4d(Cubes, 6).

%% ======================================================================
%% 3D conways game of life
%% ======================================================================

conway_3d(Cubes, 0) ->
  maps:size(Cubes);
conway_3d(Cubes, N) ->
  Cubes0 = do_conway_3d_step(Cubes),
  %% show_layers(Cubes0),
  conway_3d(Cubes0, N - 1).


do_conway_3d_step(Cubes) ->
  %% TODO find a better (smaller) bound
  Bound = get_bound(Cubes),

  Coords =
    [{X, Y, Z} ||
      X <- lists:seq(-Bound, Bound),
      Y <- lists:seq(-Bound, Bound),
      Z <- lists:seq(-Bound, Bound)],
  lists:foldl(fun(Coord, Acc) ->
                  step_one_cube(Coord, Cubes, Acc)
              end, #{}, Coords).

step_one_cube(Coord, OldCubes, NewCubes) ->
  Nbrs = get_neighbors(Coord),
  NumActiveNbrs =
    lists:foldl(
      fun(Nbr, Acc) ->
          case maps:is_key(Nbr, OldCubes) of
            true -> Acc + 1;
            _ -> Acc
          end
      end, 0, Nbrs),

  WasActive = maps:is_key(Coord, OldCubes),
  case {WasActive, NumActiveNbrs} of
    {true, N} when (N == 2) or (N == 3) ->
      maps:put(Coord, $#, NewCubes);
    {false, N} when (N == 3) ->
      maps:put(Coord, $#, NewCubes);
    _ ->
      NewCubes
  end.

get_neighbors({X, Y, Z}) ->
  [{XN, YN, ZN} ||
    XN <- [X - 1, X, X + 1],
    YN <- [Y - 1, Y, Y + 1],
    ZN <- [Z - 1, Z, Z + 1],
    not ((XN == X) and (YN == Y) and (ZN == Z))].

get_bound(Cubes) ->
  1 + maps:fold(
        fun({X, Y, Z}, _, Acc) ->
            lists:max([Acc, abs(X), abs(Y), abs(Z)])
        end, 0, Cubes).

show_layers(Cubes) ->
  Bound = get_bound(Cubes),

  io:format("~nLayers~n"
            "=========================================~n"
            "Active cubes: ~p~n"
           "Bound: ~p~n", [Cubes, Bound]),

  lists:foreach(
    fun(Z) ->
        io:format("~n", []),
        Slice = maps:fold(
                  fun({X, Y, Z0}, _, Slice) ->
                      if Z == Z0 ->
                          maps:put({X, Y}, $#, Slice);
                         true  ->
                          Slice
                      end
                  end, #{}, Cubes),

        case maps:size(Slice) of
          0 -> ok;
          _ ->
            io:format("Z=~p~n", [Z]),
            io:format("~s~n", [grid:to_str(Slice)])
        end
    end, lists:seq(-Bound, Bound)).

%% ======================================================================
%% 4D conways game of life (brain explodes)
%% ======================================================================

conway_4d(Cubes, 0) ->
  maps:size(Cubes);
conway_4d(Cubes, N) ->
  Cubes0 = do_conway_4d_step(Cubes),
  %% show_layers(Cubes0),
  ?debugFmt("Step ~p, active cells: ~p", [N, maps:size(Cubes0)]),
  conway_4d(Cubes0, N - 1).

do_conway_4d_step(Cubes) ->
  %% TODO find a better (smaller) bound
  Bound = get_bound_4d(Cubes),

  Coords =
    [{X, Y, Z, W} ||
      X <- lists:seq(-Bound, Bound),
      Y <- lists:seq(-Bound, Bound),
      Z <- lists:seq(-Bound, Bound),
      W <- lists:seq(-Bound, Bound)],

  lists:foldl(fun(Coord, Acc) ->
                  step_one_cube_4d(Coord, Cubes, Acc)
              end, #{}, Coords).

step_one_cube_4d(Coord, OldCubes, NewCubes) ->
  Nbrs = get_neighbors_4d(Coord),
  NumActiveNbrs =
    lists:foldl(
      fun(Nbr, Acc) ->
          case maps:is_key(Nbr, OldCubes) of
            true -> Acc + 1;
            _ -> Acc
          end
      end, 0, Nbrs),

  WasActive = maps:is_key(Coord, OldCubes),
  case {WasActive, NumActiveNbrs} of
    {true, N} when (N == 2) or (N == 3) ->
      maps:put(Coord, $#, NewCubes);
    {false, N} when (N == 3) ->
      maps:put(Coord, $#, NewCubes);
    _ ->
      NewCubes
  end.

get_neighbors_4d({X, Y, Z, W}) ->
  [{XN, YN, ZN, WN} ||
    XN <- [X - 1, X, X + 1],
    YN <- [Y - 1, Y, Y + 1],
    ZN <- [Z - 1, Z, Z + 1],
    WN <- [W - 1, W, W + 1],
    not ((XN == X) and (YN == Y) and (ZN == Z) and (WN == W))].

get_bound_4d(Cubes) ->
  1 + maps:fold(
        fun({X, Y, Z, W}, _, Acc) ->
            lists:max([Acc, abs(X), abs(Y), abs(Z), abs(W)])
        end, 0, Cubes).

show_layers_4d(Cubes) ->
  Bound = get_bound(Cubes),

  io:format("~nLayers~n"
            "=========================================~n"
            "Active cubes: ~p~n"
           "Bound: ~p~n", [Cubes, Bound]),

  lists:foreach(
    fun(Z) ->
        lists:foreach(
          fun(W) ->
              io:format("~n", []),
              Slice = maps:fold(
                        fun({X, Y, Z0, W0}, _, Slice) ->
                            if (Z == Z0) andalso (W == W0) ->
                                maps:put({X, Y}, $#, Slice);
                               true  ->
                                Slice
                            end
                        end, #{}, Cubes),

              case maps:size(Slice) of
                0 -> ok;
                _ ->
                  io:format("Z=~p, W=~p~n", [Z, W]),
                  io:format("~s~n", [grid:to_str(Slice)])
              end
          end, lists:seq(-Bound, Bound))
    end, lists:seq(-Bound, Bound)).


%% ======================================================================
%% Parser
%% ======================================================================

parse_3d(Binary) ->
  [L|_] = binary:split(Binary, <<"\n">>, [global]),
  W = byte_size(L) + 1,
  lists:foldl(fun({Pos, _}, Map) ->
                  X = Pos rem W,
                  Y = Pos div W,
                  Z = 0,
                  maps:put({X, Y, Z}, $#, Map)
              end, #{}, binary:matches(Binary, <<"#">>)).

parse_4d(Binary) ->
  [L|_] = binary:split(Binary, <<"\n">>, [global]),
  W = byte_size(L) + 1,
  lists:foldl(fun({Pos, _}, Map) ->
                  X = Pos rem W,
                  Y = Pos div W,
                  Z = 0,
                  Z = 0,
                  maps:put({X, Y, Z, W}, $#, Map)
              end, #{}, binary:matches(Binary, <<"#">>)).

%% Input reader (place downloaded input file in
%% priv/inputs/2020/input17.txt).
get_input() ->
  inputs:get_as_binary(2020, 17).

%% Tests
main_test_() ->
  Input = get_input(),

  [ {"Part 1", ?_assertEqual(242, part1(Input))}
  , {timeout, 60, {"Part 2", ?_assertEqual(2292, part2(Input))}}
  ].


test_input() ->
  <<".#.\n"
    "..#\n"
    "###\n">>.

ex1_test_() ->
  ?_assertEqual(112, part1(test_input())).

neighbors_test_() ->
  ?_assertEqual(26, length(get_neighbors({1, 1, 1}))).


%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
