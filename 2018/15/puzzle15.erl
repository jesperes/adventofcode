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
    Grid = read_grid("testinput.txt", 3),
    
    io:format("Start:~n~s~n", [grid_to_string(Grid)]),
    
    lists:foldl(
      fun(N, GridIn) ->
	      GridOut = do_round(GridIn),
	      io:format("After round ~p~n~s~n", [N, grid_to_string(GridOut)]),
	      GridOut
      end, Grid, lists:seq(1, 3)),
    
    ok.

    
do_round(Grid) ->
    move_or_attack(gb_trees:iterator(Grid#grid.units), Grid).

move_or_attack(Iter, Grid) -> 
    
    %% io:format("Grid:~n~s~n", [grid_to_string(Grid)]),

    %% On each unit's turn, it tries to (1) MOVE into range (if it
    %% isn't already) and then (2) ATTACK (if it is in range).

    case gb_trees:next(Iter) of
	none ->
	    %% No more units to move in this round.
	    Grid;
	
	{_Pos, {_Type, 0, _}, NextIter} ->

	    %% Unit was killed before it could take its turn, continue
	    %% with next unit.
	    move_or_attack(NextIter, Grid);

	{Pos, {Type, _HP, _Att} = Val, NextIter} ->

	    case move(Pos, Type, Grid) of
		no_path ->
		    %% There is no path from this unit to any enemy.
		    %% The unit cannot move, and ends it's turn.
		    io:format("~p: no path: ~p~n", [Type, Pos]),
		    move_or_attack(NextIter, Grid);

		in_range ->
		    %% Unit is already in range, continue with combat.
		    io:format("~p: unit in range: ~p~n", [Type, Pos]),
		    GridAfterCombat = combat(Pos, Val, Grid),
		    move_or_attack(NextIter, GridAfterCombat);

		{move_and_fight, NewPos} ->
		    io:format("~p: move, then fight ~p -> ~p~n", [Type, Pos, NewPos]),
		    GridAfterMove = apply_move(Pos, Val, NewPos, Grid),
		    GridAfterCombat = combat(NewPos, Val, GridAfterMove),
		    move_or_attack(NextIter, GridAfterCombat);

		{move, NewPos} ->
		    io:format("~p: moving ~p -> ~p~n", [Type, Pos, NewPos]),
		    GridAfterMove = apply_move(Pos, Val, NewPos, Grid),
		    move_or_attack(NextIter, GridAfterMove)
	    end
    end.

combat(Pos, {Type, HP, AttackPower} = Val, Grid) ->
    %% At beginning of combat, the unit considers all enemies in
    %% range, not just the one we happened to move towards.

    EnemiesInRange = 
	lists:sort(
	  lists:filter(fun({EnemyPos, {EnemyType, _, _}}) ->
			       (EnemyType =/= Type) 
				   and (manhattan_dist(Pos, EnemyPos) == 1)
		       end, gb_trees:to_list(Grid#grid.units))),
    
    io:format("Enemies in range of ~p (~p): ~p~n", [Pos, Val, EnemiesInRange]),
    
    case EnemiesInRange of
	[] ->
	    io:format("No enemies in range, combat can not be done.~n", []),
	    Grid;
	_ ->
	    MinHP = lists:min(lists:map(fun({Pos, {_, HP, _}}) ->
						HP
					end, EnemiesInRange)),
	    [{EnemyPos, {EnemyType, EnemyHP, EnemyAP}} = EnemyToAttack|_] =
		lists:filter(fun({Pos, {_, HP, _}}) -> HP == MinHP end, EnemiesInRange),
	    
	    
	    NewEnemyHP = EnemyHP - AttackPower,

	    io:format("Attacked enemy at ~p, new enemy hp = ~p~n", 
		      [EnemyPos, NewEnemyHP]),

	    NewUnits = gb_trees:update(EnemyPos, {EnemyType, NewEnemyHP, EnemyAP}, Grid#grid.units),
	    Grid#grid{units = NewUnits}
    end.

%% Apply the move; moving Pos to NewPos, and recalculating the grid
%% and iterator.
apply_move(Pos, Val, NewPos, Grid) ->
    NewUnits = gb_trees:insert(NewPos, Val, 
			       gb_trees:delete(Pos, Grid#grid.units)),
    Grid#grid{units = NewUnits}.

%%% Find shortest path

adjacent({Y, X}) ->
    [{Y - 1, X},
     {Y, X - 1},
     {Y, X + 1},
     {Y + 1, X}].

is_open(Pos, Grid) ->
    (not sets:is_element(Pos, Grid#grid.walls))
	and (not gb_trees:is_defined(Pos, Grid#grid.units)).

move(Pos, Type, Grid) ->
    Enemies = lists:filter(fun({_, {EnemyType, _, _}}) ->
    				   EnemyType =/= Type
    			   end, gb_trees:to_list(Grid#grid.units)), 

    %% io:format("Enemies of ~p: ~p~n", [Pos, Enemies]),

    %% The paths returned from astar2:astar/5 are on the form {Len,
    %% [{Y, X}, ...]} where {Y, X} is the square adjacent to the
    %% enemy. This is fortunate (or really by design), because it
    %% means that sorting the list of paths, the path we are heading
    %% for towards the enemy ends up at the beginning of the list.
    Paths = 
	lists:sort(lists:flatten(
		     [path_to_enemy(Pos, Enemy, Grid) 
		      || Enemy <- Enemies])),
    
    %% io:format("Best paths towards enemies: ~p~n", [Paths]),

    case Paths of
	[] -> 
	    %% This means that there is no path to any enemy for the
	    %% unit at this position; the unit cannot move.
	    no_path;
	[{_, BestPath}|_] ->
	    %% Now, reverse the list to find out which square to move
	    %% to. The first element in the reversed list is our
	    %% starting position.
	    case BestPath  of
		[_AdjPos] ->
		    %% Already in range of enemy
		    in_range;
		[Next, _AdjPos] ->
		    %% Moving one step will bring us in range for combat
		    {move_and_fight, Next};
		_ -> 
		    %% Unit is too far, can just move.
		    [_StartPos, Next|_RemainingPathTowardsEnemy] = 
			lists:reverse(BestPath),
		    {move, Next}
	    end
    end.


path_to_enemy(Pos, {EnemyPos, _}, Grid) ->
    case manhattan_dist(Pos, EnemyPos) of
	1 ->
	    %% If the positions are adjacent, don't bother searching.
	    {1, [Pos]};
	_ ->
	    EnemyAdj = open_and_adjacent(EnemyPos, Grid),
	    Paths = lists:filter(fun(P) ->
					 P =/= search_exhausted
				 end, [find_path(Pos, AdjPos, Grid)
				       || AdjPos <- EnemyAdj]),
	    Paths
    end.

manhattan_dist({X1, Y1}, {X2, Y2}) ->   
    abs(X1 - X2) + abs(Y1 - Y2).

open_and_adjacent(Pos, Grid) ->
    lists:filter(
      fun(AdjPos) ->
	      is_open(AdjPos, Grid)
      end, adjacent(Pos)).

find_path(StartPos, EndPos, Grid) ->
    {Ye, Xe} = EndPos,
    
    CostFn = fun({Y, X}) -> abs(Xe - X) + abs(Ye - Y) end,
    DistFn = fun(_, _) -> 1 end,
    NbrFn = 
	fun(Pos) ->
		open_and_adjacent(Pos, Grid)
	end,
    
    astar2:astar(StartPos, EndPos, CostFn, NbrFn, DistFn).


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

%%% Tests

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
    

move_test() ->
    Grid = read_grid("testinput2.txt", 3),

    ?assertEqual(in_range, move({1, 3}, 'E', Grid)),    
    ?assertEqual({move_and_fight, {1, 3}}, move({1, 2}, 'E', Grid)),
    ?assertEqual({move, {1, 2}}, move({1, 1}, 'E', Grid)).

move2_test() ->     
    Bin = <<"#########\n",
	    "#.G...G.#\n",
	    "#...G...#\n",
	    "#...E..G#\n",
	    "#.G.....#\n",
	    "#.......#\n",
	    "#G..G..G#\n",
	    "#.......#\n",
	    "#########\n">>,
    Grid = parse_grid(Bin, 0),
    ?assertEqual(in_range, move({2, 4}, 'G', Grid)).
    
    
