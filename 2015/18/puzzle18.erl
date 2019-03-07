%%% @author  <jespe@LAPTOP-P6HKA27J>
%%% @copyright (C) 2018, 
%%% @doc
%%%
%%% @end
%%% Created : 23 Dec 2018 by  <jespe@LAPTOP-P6HKA27J>

-module(puzzle18).
-export([start/0]).

start() ->
    {run(false), run(true)}.

run(CornersAlwaysOn) ->
    Bounds = {100, 100},
    Grid = input("input.txt", 101, Bounds, CornersAlwaysOn),
    G0 = lists:foldl(fun(_N, G) ->
			     next_state(G, Bounds, CornersAlwaysOn)
		     end, Grid, lists:seq(1, 100)),
    maps:size(G0).
   
match({Pos, _}, Width) ->
    {Pos rem Width, Pos div Width}.

input(Filename, Width, Bounds, CornersAlwaysOn) ->
    {ok, Binary} = file:read_file(Filename),
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
	{_, false, _} -> false;
	{_, _, _} -> false
    end.
