-module(aoc2015_day18).

-include_lib("eunit/include/eunit.hrl").

main_test_() ->
  [ {"Part 1", fun() -> ?assertEqual(768, run(false)) end}
  , {"Part 2", fun() -> ?assertEqual(781, run(true)) end}
  ].

run(CornersAlwaysOn) ->
  Bounds = {100, 100},
  Grid = input(101, Bounds, CornersAlwaysOn),
  G0 = lists:foldl(fun(_N, G) ->
                       next_state(G, Bounds, CornersAlwaysOn)
                   end, Grid, lists:seq(1, 100)),
  maps:size(G0).

match({Pos, _}, Width) ->
  {Pos rem Width, Pos div Width}.

input(Width, Bounds, CornersAlwaysOn) ->
  Binary = inputs:get_as_binary(2015, 18),
  maps:from_list([{match(Match, Width), true} ||
                   Match <- binary:matches(Binary, <<"#">>)]
                 ++ corners(Bounds, CornersAlwaysOn)).

corners(_, false) ->
  [];
corners({MaxX, MaxY}, true) ->
  [{0, 0},
   {MaxX - 1, 0},
   {0, MaxY - 1},
   {MaxX - 1, MaxY - 1}].

adjacent({X, Y}, {MaxX, MaxY}) ->
  [{Xa, Ya} ||
    Xa <- lists:seq(X - 1, X + 1),
    Ya <- lists:seq(Y - 1, Y + 1),
    {Xa, Ya} /= {X, Y},
    Xa >= 0,
    Ya >= 0,
    Xa < MaxX,
    Ya < MaxY].

next_state(Grid, {MaxX, MaxY} = Bounds, CornersAlwaysOn) ->
  Xs =
    [{{X, Y}, next_state0({X, Y}, Grid, Bounds, CornersAlwaysOn)} ||
      X <- lists:seq(0, MaxX - 1),
      Y <- lists:seq(0, MaxY - 1)],
  lists:foldl(fun({Pos, true}, S) ->
                  maps:put(Pos, true, S);
                 (_, S) ->
                  S
              end, #{}, Xs).

next_state0(Pos, Grid, Bounds, CornersAlwaysOn) ->
  NumAdjOn =
    length(
      lists:filter(
        fun(Adj) ->
            maps:is_key(Adj, Grid)
        end, adjacent(Pos, Bounds))),

  IsCorner = lists:member(Pos, corners(Bounds, CornersAlwaysOn)),
  IsOn = maps:is_key(Pos, Grid),

  case {IsCorner, IsOn, NumAdjOn} of
    {true, _, _} -> true;
    {_, true, 2} -> true;
    {_, true, 3} -> true;
    {_, true, _} -> false;
    {_, false, 3} -> true;
    {_, false, _} -> false
  end.
