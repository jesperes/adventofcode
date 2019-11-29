-module(adventofcode03b).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").

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

spiralmemory_add(N, Value, Coord, {Set, Map, MapC}) when is_integer(N) and 
							 is_integer(Value) and
							 is_tuple(Coord)
							 ->
    {
      %% The set contains all coordinates so that we quickly can
      %% lookup if a cell is used or not.
      sets:add_element(Coord, Set), 

      %% The maps uses the index as key, and maps to the value and the
      %% coordinate.
      maps:put(N, {Value, Coord}, Map),

      %% Maps coordinate to its value
      maps:put(Coord, Value, MapC)
    }.

spiralmemory_get_element_at_index(I, {_, Map, _}) ->
    maps:get(I, Map).

spiralmemory_get_value_at_coord(Coord, {_, _, MapC}) ->
    maps:get(Coord, MapC, 0).

spiralmemory_has_coord(Coord, {Set, _, _}) ->
    sets:is_element(Coord, Set).

spiralmemory_new() ->
    {sets:new(), maps:new(), maps:new()}.

spiralmemory_get_new_value_at({X,Y}, SpiralMemory) ->
    {_List, Value} = 
	lists:mapfoldr(
	  fun({Dx,Dy}, AccIn) ->
		  DCoord = {X + Dx, Y + Dy},
		  case spiralmemory_get_value_at_coord(DCoord, SpiralMemory) of
		      {badmap, _} ->
			  {0, AccIn};
		      Value ->
			  {Value, Value + AccIn}
			  %% {{DCoord,{d,Dx,Dy},Value}, Value + AccIn}
		  end
	  end, 0, [{Dx,Dy} || Dx <- [-1,0,1], Dy <- [-1,0,1]]),
    Value.

spiralmemory_get_latest_value(SM = {_, Map, _}) ->
    LargestIndex = lists:max(maps:keys(Map)),
    {Value, _} = spiralmemory_get_element_at_index(LargestIndex, SM),
    Value.

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
    SM = spiral(Init, N, spiralmemory_new(), initial_direction()),
    spiralmemory_get_latest_value(SM).

%%% spiral/4: collect a map of spiral elements starting at 1 and
%%% ending at N.
spiral(1, N, SpiralMemory, Delta) ->
    %% Starting coordinates is {0, 0}
    spiral(2, N, spiralmemory_add(1, 1, {0, 0}, SpiralMemory), Delta);
spiral(I, N, SpiralMemory, _) when I > N ->
    SpiralMemory;
spiral(I, N, SpiralMemory, Delta) ->
    %% io:format("Step ~w~n", [I]),

    %% Coordinate of the previous cell
    {_, Coord} = spiralmemory_get_element_at_index(I - 1, SpiralMemory),
    
    %% The new coordinate + delta if we rotate
    {NewCoordRotated, NewDeltaRotated} = get_next_coord_rotated(Coord, Delta),
    
    %% New coordinate + delta if we go straight. Note that
    %% the delta is same as before.
    {NewCoordStraight, Delta} = get_next_coord_forward(Coord, Delta),
    
    CanRotate = not spiralmemory_has_coord(NewCoordRotated, SpiralMemory),
    
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
    
    NewValue = spiralmemory_get_new_value_at(ActualNewCoord, SpiralMemory),    
    NewSpiralMemory = spiralmemory_add(I, NewValue, ActualNewCoord, SpiralMemory),
    
    if NewValue > N ->
	    % io:format("NewValue ~w > N ~w~n, exiting", [NewValue, N]),
	    NewSpiralMemory;
       true ->
	    spiral(I + 1, N, NewSpiralMemory, ActualNewDelta)
    end.


