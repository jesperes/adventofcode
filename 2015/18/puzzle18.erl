%%% @author  <jespe@LAPTOP-P6HKA27J>
%%% @copyright (C) 2018, 
%%% @doc
%%%
%%% @end
%%% Created : 23 Dec 2018 by  <jespe@LAPTOP-P6HKA27J>

-module(puzzle18).
-compile([export_all]).

%% 2330 is too high
%% 4934 is too high

start() ->
    %% Bounds = {6, 6},
    %% Grid = input("testinput.txt", 8, Bounds),

    Bounds = {100, 100},
    Grid = input("input.txt", 101, Bounds),
    
    G0 = lists:foldl(fun(_N, G) ->
			     next_state(G, Bounds)
		     end, Grid, lists:seq(1, 100)),
    
    print_grid(G0, Bounds),
    sets:size(G0).


char_at(Pos, Grid) ->
    case sets:is_element(Pos, Grid) of
	true -> $#;
	false -> $.
    end.

print_grid(Grid, {MaxX, MaxY}) ->
    L = [[char_at({X, Y}, Grid) || X <- lists:seq(0, MaxX - 1)] ++ "\n" 
	 || Y <- lists:seq(0, MaxY - 1)],
    io:format("~s~n", [L]).
   
match({Pos, _}, Width) ->
    {Pos rem Width, Pos div Width}.

input(Filename, Width, Bounds) ->
    {ok, Binary} = file:read_file(Filename),
    %% Grid is represented as set of lit (#) coordinates.
    sets:from_list([match(Match, Width) ||
		       Match <- binary:matches(Binary, <<"#">>)] 
		   ++ corners(Bounds)).

corners({MaxX, MaxY}) ->
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
	
next_state(Grid, {MaxX, MaxY} = Bounds) ->
    Xs = 
	[{{X, Y}, next_state0({X, Y}, Grid, Bounds)} ||
	    X <- lists:seq(0, MaxX - 1),
	    Y <- lists:seq(0, MaxY - 1)],
    lists:foldl(fun({Pos, true}, S) ->
			sets:add_element(Pos, S);
		   (_, S) ->
			S
		end, sets:new(), Xs).

next_state0(Pos, Grid, Bounds) ->
    NumAdjOn = 
	length(
	  lists:filter(
	    fun(Adj) ->
		    sets:is_element(Adj, Grid)
	    end, adjacent(Pos, Bounds))),
    
    IsCorner = lists:member(Pos, corners(Bounds)),     
    IsOn = sets:is_element(Pos, Grid),

    case {IsCorner, IsOn, NumAdjOn} of
	{true, _, _} -> true;
	{_, true, 2} -> true;
	{_, true, 3} -> true;
	{_, true, _} -> false;
	{_, false, 3} -> true;
	{_, false, _} -> false;
	{_, _, _} -> false
    end.
