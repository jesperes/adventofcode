%%% @author Jesper Eskilson <jesper.eskilson@klarna.com>
%%% @copyright (C) 2019, Jesper Eskilson
%%% @doc
%%%
%%% @end
%%% Created :  2 Feb 2019 by Jesper Eskilson <jesper.eskilson@klarna.com>

-module(puzzle15).
-export([main/0]).
-compile([export_all]).

-include_lib("eunit/include/eunit.hrl").

-define(UNIT_HP, 200).
-define(GOBLIN_ATTACK_POWER, 3).

-record(grid, {
	  walls = sets:new(),
	  units = maps:new(),
	  width = 0,
	  height = 0
	 }).

main() ->
    {{part1, part1()},
     {part2, 0}}.

part1() ->    
    Grid = read_grid("testinput2.txt", 3),
    io:format("~s~n", [grid_to_string(Grid)]).
    
do_round(Grid) ->
    move_or_attack(gb_trees:iterator(Grid#grid.units), Grid).

move_or_attack(Iter, Grid) -> 

    %% On each unit's turn, it tries to (1) MOVE into range (if it
    %% isn't already) and (2) ATTACK (if it is in range).

    case gb_trees:next(Iter) of
	none ->
	    Grid;
	
	{Pos, {Type, 0}, NextIter} ->

	    %% Unit was killed before it could take its turn, continue
	    %% with next unit.
	    move_or_attack(NextIter, Grid);

	{Pos, {Type, HP}, NextIter} ->
	    
	    %%io:format("Unit = ~p~n", [Unit]),
	    %%io:format("Enemies = ~p~n", [gb_sets:to_list(Enemies)]),
	    
	    Enemies = lists:filter(fun({_, {EnemyType, _}}) ->
					   EnemyType =/= Type
				   end, gb_trees:to_list(Grid#grid.units)),
	    
	    case length(Enemies) of
		0 ->
		    {winner, elves};
		_NumEnemies ->
		    %% Find path to nearest enemy
		    io:format("Walls: ~p~n", [sets:to_list(Grid#grid.walls)]),
		    io:format("Units: ~p~n", [gb_trees:to_list(Grid#grid.units)])
		    
		    %% EnemyAdj = 
		    %% 	[{Enemy, open_and_adjacent(Enemy, Unit, Units, Walls)} ||
		    %% 	    Enemy <- gb_sets:to_list(Enemies)],
		    
		    %% io:format("Cells adjacent to enemy ~p~n", [EnemyAdj])
		    
		    %% Print all different paths
		    %% lists:foreach(
		    %%   fun ({Enemy, search_exhausted}) ->
		    %% 	      %% No path to enemy
		    %% 	      ok;
		    %% 	  ({Enemy, {_, P}}) ->
		    %% 	      PathElems = gb_sets:from_list(lists:map(fun({Y, X}) -> {Y, X, '+'} end, P)),
		    %% 	      ElemsToShow = 
		    %% 		  gb_sets:union([Walls, PathElems,
		    %% 				 gb_sets:singleton(Unit),
		    %% 				 gb_sets:singleton(Enemy)]),
		    %% 	      io:format("~s~n", [to_xy_string(ElemsToShow)])
		    %%   end, Paths) 
			      
	    end
    end.

%%% Find shortest path

adjacent({Y, X}) ->
    [{Y - 1, X},
     {Y, X - 1},
     {Y, X + 1},
     {Y + 1, X}].

is_open({Yp, Xp}, Units, Walls) ->
    gb_sets:size(gb_sets:filter(
		   fun({Y, X, _}) ->
			   (X == Xp) and (Y == Yp)
		   end, gb_sets:union([Units, Walls]))) == 0.

open_and_adjacent(Pos, EnemyPos, Units, Walls) ->
    lists:filter(
      fun(AdjPos) ->
	      if AdjPos =:= EnemyPos -> true;
		 true -> is_open(AdjPos, Units, Walls)
	      end
      end, adjacent(Pos)).

find_path(Unit, Enemy, Units, Walls) ->
    {Yu, Xu, _} = Unit,
    {Ye, Xe, _} = Enemy,
    
    %%io:format("Unit = ~p~n", [Unit]),
    %%io:format("Enemy = ~p~n", [Enemy]),

    CostFn = fun({Y, X}) -> 
		     abs(Xe - X) + abs(Ye - Y) 
	     end,
    
    NbrFn = 
	fun(Pos) ->
		Adj = open_and_adjacent(Pos, {Ye, Xe}, Units, Walls),
		%% io:format("~p -> ~p~n", [Pos, Adj]),
		Adj
	end,
    
    DistFn = fun(_, _) -> 1 end,
 
    astar2:astar({Yu, Xu}, {Ye, Xe}, CostFn, NbrFn, DistFn).


%%% Pretty-printing

grid_pos_to_string(Pos, Grid) ->
    case sets:is_element(Pos, Grid#grid.walls) of
	true -> '#';
	false -> 
	    case gb_trees:lookup(Pos, Grid#grid.units) of
		{value, {Type, _, _}} ->
		    Type;
		none -> 
		    '.'
	    end
    end.
    
lookup_test() ->
    Tree = 
	gb_trees:from_orddict([{{1,1}, {'E',200,3}},
			       {{1,4}, {'G',200,3}},
			       {{3,2}, {'G',200,3}},
			       {{3,5}, {'G',200,3}}]),
    
    Grid = read_grid("testinput2.txt", 3),

    ?assertEqual(lists:sort(gb_trees:to_list(Tree)), 
		 lists:sort(gb_trees:to_list(Grid#grid.units))),
    
    ?assert(gb_trees:is_defined({1, 1}, Tree)),
    ?assert(gb_trees:is_defined({3, 5}, Tree)),

    ?assert(gb_trees:is_defined({1, 1}, Grid#grid.units)),
    ?assert(gb_trees:is_defined({3, 5}, Grid#grid.units)),

    ?assertMatch({value, _}, gb_trees:lookup({3, 5}, Tree)),
    
    ?assertEqual('G', grid_pos_to_string({3, 5}, Grid)).
    

grid_to_string(Grid) ->
    AllPos = 
	sets:to_list(Grid#grid.walls) ++
	gb_trees:keys(Grid#grid.units),
    
    Ys = lists:map(fun({Y, _}) -> Y end, AllPos),
    Xs = lists:map(fun({_, X}) -> X end, AllPos),
    
    MinX = lists:min(Xs),
    MaxX = lists:max(Xs),
    MinY = lists:min(Ys),
    MaxY = lists:max(Ys),
    
    [[io_lib:format("~s", 
		    [grid_pos_to_string({Y, X}, Grid)]) ||
	 X <- lists:seq(MinX, MaxX)] ++ "\n" ||
	Y <- lists:seq(MinY, MaxY)].

%%% Parser

parse_grid(Binary, ElfAttackPower) ->
    Str = binary_to_list(Binary),
    [First|_] = Lines = string:tokens(Str, "\r\n"),
    
    Fun = fun(X, Y, C, L) ->
		  [{Y, X, list_to_atom([C])}|L]
	  end,
    
    {_, Items} = 
	lists:foldl(
	  fun(Line, {Y, Acc}) ->
		  {_, _, AccOut} = 
		      lists:foldl(
			fun(C, {X, Y0, InnerAcc}) ->
				{X + 1, Y0, Fun(X, Y0, C, InnerAcc)}
			end, {0, Y, Acc}, Line),
		  {Y + 1, AccOut}
	  end, {0, []}, Lines),

    Units = lists:filter(fun({_, _, C}) ->
				 (C =:= 'E') or (C =:= 'G')
			 end, Items),
    
    Walls = lists:filter(fun({_, _, C}) ->
				 C =:= '#'
			 end, Items),

    

    #grid{
       units = gb_trees:from_orddict(
		 lists:sort(
		   lists:map(fun({Y, X, C}) ->
				     {{Y, X}, {C, ?UNIT_HP, unit_attack_power(C, ElfAttackPower)}}
			     end, Units))),
       
       walls = sets:from_list(
		 lists:map(fun({Y, X, _}) ->
				   {Y, X}
			   end, Walls)),

       width = length(First),
       height = length(Lines)
      }.

read_grid(Filename, ElfAttackPower) ->
    {ok, Binary} = file:read_file(Filename),
    parse_grid(Binary, ElfAttackPower).

%%% Utilities

unit_attack_power('E', ElfAttackPower) -> ElfAttackPower;
unit_attack_power('G', _) -> ?GOBLIN_ATTACK_POWER.

     
