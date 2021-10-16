-module(aoc2017_day03_part1).

-export([spiral/1]).

%% Y grows downwards (south)
initial_direction() ->
  {0,-1}.

rotate_clockwise({1,0}) -> % East -> South
  {0,1};
rotate_clockwise({0,1}) -> % South -> West
  {-1,0};
rotate_clockwise({-1,0}) -> % West -> North
  {0,-1};
rotate_clockwise({0,-1}) -> % North -> East
  {1,0}.


%%% ------------------------------------------------------------
%%%
%%% Primitives for representing the spiral memory
%%%

spiralmemory_add(N, Coord, {Set, Map}) ->
  {sets:add_element(Coord, Set), maps:put(N, Coord, Map)}.

spiralmemory_get_coords_at_index(I, {_, Map}) ->
  maps:get(I, Map).

spiralmemory_has_coord(Coord, {Set, _}) ->
  sets:is_element(Coord, Set).

spiralmemory_new() ->
  {sets:new(), maps:new()}.

%%% ------------------------------------------------------------

get_next_coord_rotated({X,Y}, Delta) ->
  NewDelta = {XD, YD} = rotate_clockwise(Delta),
  NewCoord = {X + XD, Y + YD},
  {NewCoord, NewDelta}.

get_next_coord_forward({X,Y}, Delta) ->
  {XD, YD} = Delta,
  NewCoord = {X + XD, Y + YD},
  {NewCoord, Delta}.

%%%
%%% Compute the location of the Nth spiral element, and return
%%% its manhattan distance from the center.
spiral(N) when N >= 1 ->
  Init = 1,
  Elems = spiral(Init, N, spiralmemory_new(), initial_direction()),
  {X, Y} = spiralmemory_get_coords_at_index(N, Elems),
  abs(X) + abs(Y).

%%% spiral/4: collect a map of spiral elements starting at 1 and
%%% ending at N.
spiral(1, N, Map, Delta) ->
  %% Starting coordinates is {0, 0}
  spiral(2, N, spiralmemory_add(1, {0, 0}, Map), Delta);
spiral(I, N, Map, _) when I > N ->
  Map;
spiral(I, N, Map, Delta) ->
  %% Coordinate of the previous cell
  Coord = spiralmemory_get_coords_at_index(I - 1, Map),

  %% The new coordinate + delta if we rotate
  {NewCoordRotated, NewDeltaRotated} = get_next_coord_rotated(Coord, Delta),

  %% New coordinate + delta if we go straight. Note that
  %% the delta is same as before.
  {NewCoordStraight, Delta} = get_next_coord_forward(Coord, Delta),

  %% TODO this is inefficient since we recompute the list of map
  %% values for each iteration.
  CanRotate = not spiralmemory_has_coord(NewCoordRotated, Map),

  %% Compute the coordinates of the new cell, and the new delta.
  {ActualNewCoord, ActualNewDelta} =
    if CanRotate ->
        %% If the cell at the rotated position is already occupied,
        %% continue in the same direction as before.
        {NewCoordRotated, NewDeltaRotated};
       true ->
        %% Otherwise, continue in the direction of the rotated
        %% coordinates
        {NewCoordStraight, Delta}
    end,

  spiral(I + 1, N, spiralmemory_add(I, ActualNewCoord, Map), ActualNewDelta).
