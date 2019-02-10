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
    Grid = read_grid("input.txt", 3),
    io:format("Start:~n~s~n", [grid_to_string(Grid)]),    
    do_battle_until_death(1, Grid).

do_battle_until_death(2, Grid) ->
    io:format("Terminating at 2 rounds:~n~s~n", [grid_to_string(Grid)]),
    io:format("Units: ~w~n", [gb_trees:to_list(Grid#grid.units)]);
do_battle_until_death(N, Grid) ->
    io:format("Doing round ~w: ~w~n", [N, gb_trees:to_list(Grid#grid.units)]),
    case do_round(Grid) of
        {winner, Type, FinalGrid} ->
            FullRounds = N - 1,
	    SumHP = sum_hp(FinalGrid),

            io:format("Winner is ~p after ~p full rounds, final grid:~n~s~n",
                      [Type, N - 1, grid_to_string(FinalGrid)]),
            io:format("Output = ~p * ~p = ~p~n", 
                      [FullRounds, SumHP, FullRounds * SumHP]),
	    FullRounds * SumHP;
        NewGrid ->
            %% io:format("~nAfter ~p rounds:~n~s~n", [N, grid_to_string(NewGrid)]),
            do_battle_until_death(N + 1, NewGrid)
    end.

sum_hp(Grid) ->
    %% io:format("~p~n", [gb_trees:to_list(Grid#grid.units)]),
    lists:sum(lists:map(fun({_, {_, HP, _}}) -> 
				HP
			end, 
                        gb_trees:to_list(Grid#grid.units))).

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
	
	{Pos, {Type, _, _}, NextIter} ->
            
            %% io:format("~n=======[ ~p (~p) ]========~n", [Pos, Type]),

            %% Note that we cannot use the HP received from the
            %% iterator since the HP may have been modified due to
            %% attacks by other units.
            
	    case gb_trees:lookup(Pos, Grid#grid.units) of
		none ->
                    %% Unit was killed before its turn.
                    %% io:format("~p unit at ~p was killed before its turn.~n", [Type, Pos]),
                    move_or_attack(NextIter, Grid);

		{value, _} ->
		    case move(Pos, Type, Grid) of
			no_enemies ->
			    %% There are no enemies left,
			    {winner, Type, Grid};
			
			no_path ->
			    %% There is no path from this unit to any enemy.
			    %% The unit cannot move, and ends it's turn.
			    %% io:format("~p unit at ~p can not reach any enemy~n", [Type, Pos]),
			    move_or_attack(NextIter, Grid);
                
			in_range ->
			    %% Unit is already in range, continue with combat.
			    %% io:format("~p unit at ~p can combat enemy units directly~n", 
			    %%           [Type, Pos]),
			    GridAfterCombat = combat(Pos, Grid),
			    move_or_attack(NextIter, GridAfterCombat);
				
			{move_and_fight, NewPos} ->
			    %% io:format("~p unit at ~p can move to ~p and fight~n", [Type, Pos, NewPos]),
			    GridAfterMove = apply_move(Pos, NewPos, Grid),
			    GridAfterCombat = combat(NewPos, GridAfterMove),
			    move_or_attack(NextIter, GridAfterCombat);
			
			{move, NewPos, BestPath} ->
			    %% io:format("~p unit at ~p moves to ~p (path to enemy: ~w)~n", [Type, Pos, NewPos, BestPath]),
			    GridAfterMove = apply_move(Pos, NewPos, Grid),
			    move_or_attack(NextIter, GridAfterMove)
		    end
	    end
    end.

combat(Pos, Grid) ->
    %% At beginning of combat, the unit considers all enemies in
    %% range, not just the one we happened to move towards.

    {Type, _HP, AttackPower} = gb_trees:get(Pos, Grid#grid.units),

    EnemiesInRange = 
	lists:sort(
	  lists:filter(fun({EnemyPos, {EnemyType, _, _}}) ->
			       (EnemyType =/= Type) 
				   and (manhattan_dist(Pos, EnemyPos) == 1)
		       end, gb_trees:to_list(Grid#grid.units))),
    
    
    %%io:format("Enemies in range of ~p (~p): ~p~n", [Pos, Val, EnemiesInRange]),
    
    case EnemiesInRange of
	[] ->
	    %%io:format("No enemies in range, combat can not be done.~n", []),
	    Grid;
	_ ->
	    MinHP = lists:min(lists:map(fun({_, {_, HP, _}}) ->
						HP
					end, EnemiesInRange)),
	    [{EnemyPos, {EnemyType, EnemyHP, EnemyAP}}|_] =
		lists:filter(fun({_, {_, HP, _}}) -> HP == MinHP end, EnemiesInRange),
	    
	    
	    NewEnemyHP = EnemyHP - AttackPower,

	    %% io:format("Attacked enemy at ~p, new enemy hp = ~p~n", 
	    %%           [EnemyPos, NewEnemyHP]),
            
	    %% Delete enemy unit if dead, otherwise update its HP
	    NewUnits = 
		if NewEnemyHP =< 0 ->
			io:format("Deleted dead unit ~p at ~p, ~p units left.~n", 
				  [EnemyType, EnemyPos, gb_trees:size(Grid#grid.units) - 1]), 
			gb_trees:delete(EnemyPos, Grid#grid.units);
		   true ->
			gb_trees:update(EnemyPos, {EnemyType, NewEnemyHP, EnemyAP}, Grid#grid.units)
		end,

	    Grid#grid{units = NewUnits}
    end.

%% Apply the move; moving Pos to NewPos, and recalculating the grid
%% and iterator.
apply_move(Pos, NewPos, Grid) ->
    Val = gb_trees:get(Pos, Grid#grid.units),
    
    %% case gb_trees:lookup(NewPos, Grid#grid.units) of
    %% 	none ->
    NewUnits = gb_trees:insert(NewPos, Val, 
			       gb_trees:delete(Pos, Grid#grid.units)),
    Grid#grid{units = NewUnits}.
    %% 	{value, V} ->
    %% 	    io:format("~s~n", [grid_to_string(Grid)]),
    %% 	    false = 
    %% 		io:format("Attempting to move unit ~p at ~p into ~p which is occupied by ~p~n",
    %% 			  [Val, Pos, NewPos, V])
    %% end.
	    

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

    io:format("~s~n", [grid_to_string(Grid)]),

    %% EnemySquares is the list of all enemy positions
    EnemySquares = lists:filtermap(fun({EnemyPos, {EnemyType, _, _}}) ->
					   if (EnemyType =/= Type) ->
						   {true, EnemyPos};
					      true -> 
						   false
					   end
				   end, gb_trees:to_list(Grid#grid.units)), 
    
    case EnemySquares of 
	[] ->
	    %% There are no more enemies left.
	    no_enemies;
	
	_ ->
	    
	    %% InRange is the list of all squares in range of at least one enemy
	    InRange = [Adj ||
			  Enemy <- EnemySquares,
			  Adj <- adjacent(Enemy),
			  is_open(Adj, Grid) or (Adj =:= Pos)],
	    
	    %% io:format("Positions in range of at least one enemy: ~p~n", [InRange]),
	    
	    %% Compute the list of squares adjacent to enemies with the shortest
	    %% distance.
	    {BestReachable, _} =
		lists:foldl(fun(P, {Paths, Best} = Acc) ->
				    %% TODO prune the search tree of paths
				    %% known to be too long.
				    Path = find_path(Pos, P, Best, Grid),
				    case Path of
					{Len, _} when Len < Best ->
					    {[Path], Len};
					{Len, _} when Len =:= Best ->
					    {[Path|Paths], Best};
					_ ->
					    Acc
				    end
					
			    end, {[], inf}, InRange),
	    
	    case BestReachable of
		[] ->
		    %% There are no enemies reachable from this unit.
		    no_path;
		
		_ ->
		    %% The target square (adjacent to enemy) will be first in
		    %% the paths returned by the A* implementation, so by
		    %% sorting them we will get them in the right order.
		    %% 
		    %% "Nearest" has the same meaning as in the puzzle
		    %% description, i.e. those enemy-adjacent squares which
		    %% all share the smallest distance to the attacking unit.
		    %% Of these, we choose the first one in reading order
		    %% (this will be the first one in the "Nearest" list).
		    %% 
		    %% "Chosen" is here the enemy-adjacent, reachable, square
		    %% which we chose. 
		    [{_Len, [Chosen|_]}|_] = Nearest = lists:sort(BestReachable),
		    
		    %% Note that there may be more than one path which leads
		    %% to the square we chose to head for, so we need to look through
		    %% all paths, and take the 
		    [NewPos|_] =
			lists:sort(
			  lists:filtermap(fun({_, [X|_] = Path}) ->
						  if X =:= Chosen ->
							  [Start|_] = lists:reverse(Path),
							  {true, Start};
						     true ->
							  false
						  end
					  end, Nearest)),
		    
		    %% io:format("All start squares: ~p~n", [AllStartSquares]),
		    NewPos
	    end
    end.

    %% X = [find_shortest_paths(Pos, End, Grid) ||
    %% 	    End <- InRange

    %% io:format("Enemies of ~p: ~p~n", [Pos, Enemies]),

    %% The paths returned from astar2:astar/5 are on the form {Len,
    %% [{Y, X}, ...]} where {Y, X} is the square adjacent to the
    %% enemy. This is fortunate (or really by design), because it
    %% means that sorting the list of paths, the path we are heading
    %% for towards the enemy ends up at the beginning of the list.
    %%  Paths = 
    %% 	lists:sort(lists:flatten(
    %% 		     [path_to_enemy(StartPos, Enemy, Grid)
    %% 		      || StartPos <- StartSquares,
    %% 			 Enemy <- Enemies])),

    %% P0 = {11, 13},
    
    %% if Pos =:= P0 ->
    %%  	    lists:foreach(
    %% 	      fun(P) ->
    %% 		      io:format("~p = ~w~n", [Pos, P])
    %% 	      end, Paths);
    %%    true -> ok
    %% end,
    
    %% case {Enemies, Paths} of
    %%     {[], _} ->
    %%         %% No enemies found, end of combat.
    %%         no_enemies;
    %% 	{_, []} -> 
    %% 	    %% This means that there is no path to any enemy for the
    %% 	    %% unit at this position; the unit cannot move.
    %% 	    no_path;
    %% 	{_, [{Steps, _}|_] = AllPaths} ->
	    
    %% 	    %% The best path to any square adjacent to an enemy is 
    %% 	    %% Steps long, but there may several such paths.

    %% 	    [BestPath|_] = AllPaths0 =
    %% 		lists:sort(
    %% 		  lists:filtermap(fun({N, Path}) ->
    %% 					  if N == Steps ->
    %% 						  {true, lists:reverse(Path)};
    %% 					     true ->
    %% 						  false
    %% 					  end
    %% 				  end, AllPaths)),
	    
    %% 	    if Pos =:= P0 ->
    %% 	     	    %% io:format("All paths from ~p unit at ~p to enemies:~n~p~n", [Type, Pos, AllPaths0]),
    %% 	     	    io:format("Selecting: ~p~n", [BestPath]);
    %% 	       true ->
    %% 	     	    ok
    %% 	    end,

    %% 	    case BestPath of
    %% 	    	[] ->
    %% 	    	    %% Already in range of enemy
    %% 	    	    in_range;
    %% 	    	[Next] ->
    %% 	    	    %% Moving one step will bring us in range for combat
    %% 	    	    {move_and_fight, Next};
    %% 	    	_ -> 
    %% 	    	    %% Unit is too far, can just move.
    %% 	    	    [Next|_RemainingPathTowardsEnemy] = BestPath,
    %% 	    	    {move, Next, BestPath}
    %% 	    end
    %% end.

%% path_to_enemy(Pos, {Pos, _}, _) ->
%%     {0, []};
%% path_to_enemy(Pos, {EnemyPos, _}, Grid) ->
%%     %% io:format("Searching path from ~p to ~p~n", [Pos, EnemyPos]),
%%     EnemyAdj = open_and_adjacent(EnemyPos, Grid),
%%     Paths = lists:filter(fun(P) ->
%% 				 P =/= search_exhausted
%% 			 end, [find_path(Pos, AdjPos, Grid)
%% 			       || AdjPos <- EnemyAdj]),
%%     Paths.
	
manhattan_dist({X1, Y1}, {X2, Y2}) ->   
    abs(X1 - X2) + abs(Y1 - Y2).

open_and_adjacent(Pos, Grid) ->
    lists:filter(
      fun(AdjPos) ->
	      is_open(AdjPos, Grid)
      end, adjacent(Pos)).

find_path(StartPos, EndPos, _Best, Grid) ->
    %% Best is here the current shortest distance, and we are not
    %% interested in any solutions worse than that, but our astar
    %% implementation does not support specifying an upper bound.
    
    {Ye, Xe} = EndPos,
    
    CostFn = fun({Y, X}) -> abs(Xe - X) + abs(Ye - Y) end,
    DistFn = fun(_, _) -> 1 end,
    NbrFn = 
	fun(Pos) ->
		open_and_adjacent(Pos, Grid)
	end,
    
    astar2:astar(StartPos, EndPos, CostFn, NbrFn, DistFn).

%% get_start_squares(Pos, Type, Grid) ->
%%     lists:filter(fun(Adj) ->
%% 			 case sets:is_element(Adj, Grid#grid.walls) of
%% 			     true ->
%% 				 %% Not a wall
%% 				 false;
%% 			     false ->
%% 				 %% Lookup = gb_trees:lookup(Adj, Grid#grid.units),
%% 				 %% io:format("Lookup = ~p~n", [Lookup]),
%% 				 case gb_trees:lookup(Adj, Grid#grid.units) of
%% 				     {value, {T, _, _}} when (T =/= Type) ->
%% 					 %% Enemy
%% 					 true; 
%% 				     {value, _} ->
%% 					 %% Non-enemy
%% 					 false;
%% 				     none ->
%% 					 %% Open space
%% 					 true
%% 				 end
%% 			 end
%% 		 end, adjacent(Pos)).

%%% Pretty-printing

grid_pos_to_string(Pos, Grid) ->
    case sets:is_element(Pos, Grid#grid.walls) of
	true -> '#';
	false -> 
	    case gb_trees:lookup(Pos, Grid#grid.units) of
		{value, {Type, HP, _}} when HP > 0 ->
		    Type;
		_ -> 
		    '.'
	    end
    end.
    

units_and_hp(Y, Grid) ->
    Units = gb_trees:keys(Grid#grid.units),
    UnitsOnThisLine = 
        lists:filter(fun({Y0, _}) ->
                             Y0 == Y
                     end, Units),
    lists:map(
      fun(Unit) ->
              {Type, HP, _} = gb_trees:get(Unit, Grid#grid.units),
	      if HP > 0 ->
		      io_lib:format(" ~s(~w)", [Type, HP]);
		 true ->
		      []
	      end
      end, UnitsOnThisLine).

    
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
    
    [io_lib:format("~w ", [Y]) ++
     [io_lib:format("~s", 
		    [grid_pos_to_string({Y, X}, Grid)]) ||
	 X <- lists:seq(MinX, MaxX)] ++ units_and_hp(Y, Grid) ++ "\n" ||
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
    

%% move_test() ->
%%     Grid = read_grid("testinput2.txt", 3),

%%     ?assertEqual(in_range, move({1, 3}, 'E', Grid)),    
%%     ?assertEqual({move_and_fight, {1, 3}}, move({1, 2}, 'E', Grid)),
%%     ?assertEqual({move, {1, 2}}, move({1, 1}, 'E', Grid)).

move1_test() ->
    Bin = <<"#######\n",
	    "#.E...#\n",
	    "#.....#\n",
	    "#...G.#\n",
	    "#######\n">>,
    Grid = parse_grid(Bin, 0),
    move({1, 2}, 'E', Grid).

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

move3_test() ->        
    Bin = <<"#######\n",
	    "#..G..#\n",
	    "#...G.#\n",
	    "#.#G#G#\n",
	    "#...#E#\n",
	    "#.....#\n",
	    "#######\n">>,
    Grid = parse_grid(Bin, 0),
    ?assertMatch({move, {1, 2}, _}, move({1, 3}, 'G', Grid)).


move4_test() ->
    Bin = <<"#######\n",
	    "#..GE.#\n",
	    "#.....#\n",
	    "#.#.#.#\n",
	    "#...#.#\n",
	    "#.....#\n",
	    "#######\n">>,
    Grid = parse_grid(Bin, 0),
    ?assertMatch(in_range, move({1, 3}, 'G', Grid)).

ex_helper(Binary) ->
    Grid = parse_grid(Binary, 3),
    do_battle_until_death(1, Grid).

ex1_test() ->
    Bin = <<"#######\n",
	    "#G..#E#\n",
	    "#E#E.E#\n",
	    "#G.##.#\n",
	    "#...#E#\n",
	    "#...E.#\n",
	    "#######\n">>,
    ?assertEqual(36334, ex_helper(Bin)).

ex2_test() ->
    Bin = <<"#######\n",
	    "#E..EG#\n",
	    "#.#G.E#\n",
	    "#E.##E#\n",
	    "#G..#.#\n",
	    "#..E#.#\n",
	    "#######\n">>,
    ?assertEqual(39514, ex_helper(Bin)).

ex3_test() ->
    Bin = <<"#######\n",
	    "#E.G#.#\n",
	    "#.#G..#\n",
	    "#G.#.G#\n",
	    "#G..#.#\n",
	    "#...E.#\n",
	    "#######\n">>,
    ?assertEqual(27755, ex_helper(Bin)).

ex4_test() ->
    Bin = <<"#######\n",
	    "#.E...#\n",
	    "#.#..G#\n",
	    "#.###.#\n",
	    "#E#G#G#\n",
	    "#...#G#\n",
	    "#######\n">>,
    ?assertEqual(28944, ex_helper(Bin)).
    
ex5_test() ->
    Bin = <<"#########\n",
	    "#G......#\n",
	    "#.E.#...#\n",
	    "#..##..G#\n",
	    "#...##..#\n",
	    "#...#...#\n",
	    "#.G...G.#\n",
	    "#.....G.#\n",
	    "#########\n">>,
    ?assertEqual(18740, ex_helper(Bin)).
    
