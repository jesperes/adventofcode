%%% @author  <jespe@LAPTOP-P6HKA27J>
%%% @copyright (C) 2018, 
%%% @doc
%%%
%%% @end
%%% Created : 22 Dec 2018 by  <jespe@LAPTOP-P6HKA27J>

-module(puzzle22).
-include_lib("eunit/include/eunit.hrl").
-export([main/0]).
-compile([export_all]).


main() ->
    test(),

    Target = {8, 701, torch},
    Grid = compute_grid(200, 1200, 5913, Target),
    {{part1, part1(Grid)},
     {part2, part2(Grid)}}.

test() ->
    Target = {10, 10, torch},
    Grid = compute_grid(50, 50, 510, Target),
    114 = part1(Grid),
    45 = part2(Grid).

compute_grid_cell(X, Grid) ->
    Y = maps:get(y, Grid),
    Depth = maps:get(depth, Grid),
    {Xt, Yt, _} = maps:get(target, Grid),
    GI = case {X, Y} of
	     {0, 0} -> 0;
	     {X, 0} -> X * 16807;
	     {0, Y} -> Y * 48271;
	     {Xt, Yt} -> 0;
	     {X, Y} ->
		 {EL1, _} = maps:get({X, Y-1}, Grid),
		 {EL2, _} = maps:get({X-1, Y}, Grid),
		 EL1 * EL2
	 end,
    EL = (GI + Depth) rem 20183,
    RT = EL rem 3,
    Grid#{{X, Y} => {EL, RT}}.

compute_grid_line(Y, #{max_x := MaxX} = Grid) ->
    lists:foldl(fun compute_grid_cell/2, 
		Grid#{y => Y}, 
		lists:seq(0, MaxX)).

compute_grid(MaxX, MaxY, Depth, Target) ->
    lists:foldl(fun compute_grid_line/2, 
		#{max_x => MaxX, depth => Depth, target => Target}, 
		lists:seq(0, MaxY)).

part1(Grid) ->
    {Xt, Yt, _} = maps:get(target, Grid),
    Coords = [{X, Y} || 
		 X <- lists:seq(0, Xt),
		 Y <- lists:seq(0, Yt)],
    Fun = fun(Pos, N) ->
		  {_, RT} = maps:get(Pos, Grid),
		  RT + N
	  end,
    lists:foldl(Fun, 0, Coords).

region_type(0) -> rocky;
region_type(1) -> wet;
region_type(2) -> narrow.
    
tools() ->
    [climbing_gear, torch, neither].

%% We store the region type as an int, because it is computed as
%% "erosion level" mod 3. So, 0 = rocky, 1 = wet, 2 = narrow.
valid_region_type(climbing_gear, 0) -> true;
valid_region_type(climbing_gear, 1) -> true;
valid_region_type(torch, 0) -> true;
valid_region_type(torch, 2) -> true;
valid_region_type(neither, 1) -> true;
valid_region_type(neither, 2) -> true;
valid_region_type(_, _) -> false.

filter_nbr({{X, Y}, T}, Grid, Acc) when (X >= 0) and (Y >= 0) ->
    {_, RT} = maps:get({X, Y}, Grid),
    case valid_region_type(T, RT) of
	true ->
	    [{X, Y, T}|Acc];
	false ->
	    Acc
    end;
filter_nbr(_, _, Acc) ->
    Acc.
    
%% Return the list of neighbors to {X, Y, T}.
nbrs({X, Y, _T}, Grid) ->
    lists:foldl(fun(Elem, Acc) ->
			filter_nbr(Elem, Grid, Acc)
		end, [],
		[{Pos, Tool} ||
		    Tool <- tools(),
		    Pos <- [{X - 1, Y},
			    {X + 1, Y},
			    {X, Y + 1},
			    {X, Y - 1}]]).

part2(Grid) ->
    Goal = {Xt, Yt, Tt} = maps:get(target, Grid),
    Start = {0, 0, torch},
    CostFn = fun({X, Y, Tool}) -> 
		     C = abs(X - Xt) + abs(Y - Yt),
		     if Tt == Tool -> 
		     	     C;
		     	true -> 
		     	     C + 7
		     end
    	     end,
 
    NbrFn = fun(Node) ->
		    Nbrs = nbrs(Node, Grid),
		    %% erlang:display(Node),
		    Nbrs			
	    end,
    
    DistFn = fun({_, _, Tool1}, {_, _, Tool2}) -> 
    		     if Tool1 == Tool2 -> 1;
			true -> 8
		     end
    	     end,
    
    {Dist, _Path} = astar2:astar(Start, Goal, CostFn, NbrFn, DistFn),
    %% lists:foreach(fun(P) ->
    %% 			  io:format("Step: ~w~n", [P])
    %%  		  end, lists:reverse(Path)),
    Dist.
