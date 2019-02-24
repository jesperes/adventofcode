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

-define(LOG(Fmt, Args), io:format(Fmt, Args)).
%% -define(LOG(Fmt, Args), ok).

-define(UNIT_HP, 200).
-define(GOBLIN_ATTACK_POWER, 3).
-define(TRACE_FILE, "erltrace.terms").

-record(grid, {
	  walls = sets:new(),
	  units = maps:new(),
	  width = 0,
	  height = 0,
	  tracefile
	 }).

main() ->
    {{part1, part1()},
     {part2, 0}}.

part1() ->
    Grid0 = read_grid("input.txt", 3),
    {ok, IoDev} = file:open(?TRACE_FILE, [write]),
    Grid = Grid0#grid{tracefile = IoDev},
    try
	?LOG("Start:~n~s~n", [grid_to_string(Grid)]),
	do_battle_until_death(1, Grid)
    after
	ok = file:close(Grid#grid.tracefile)
    end.

trace_output(Grid, Term) ->
    case Grid#grid.tracefile of
        undefined ->
            ok;
        TraceIo ->
            io:format(TraceIo, "~w~n", [Term])
    end.


%% do_battle_until_death(4, Grid) ->
%%     ?LOG("Terminating at 2 rounds:~n~s~n", [grid_to_string(Grid)]),
%%     ?LOG("Units: ~w~n", [gb_trees:to_list(Grid#grid.units)]);
do_battle_until_death(N, Grid) ->
    ?LOG("Doing battle round ~w~n", [N]),

    trace_output(Grid,
                 {{round, N}, gb_trees:to_list(Grid#grid.units)}),

    case do_round(Grid) of
        {winner, Type, FinalGrid} ->
            FullRounds = N - 1,
	    SumHP = sum_hp(FinalGrid),

            ?LOG("Winner is ~p after ~p full rounds, final grid:~n~s~n",
                      [Type, N - 1, grid_to_string(FinalGrid)]),
            ?LOG("Output = ~p * ~p = ~p~n",
                      [FullRounds, SumHP, FullRounds * SumHP]),
	    FullRounds * SumHP;
        NewGrid ->
            %% ?LOG("~nAfter ~p rounds:~n~s~n", [N, grid_to_string(NewGrid)]),
            do_battle_until_death(N + 1, NewGrid)
    end.

sum_hp(Grid) ->
    %% ?LOG("~p~n", [gb_trees:to_list(Grid#grid.units)]),
    lists:sum(lists:map(fun({_, {_, HP, _}}) ->
				HP
			end,
                        gb_trees:to_list(Grid#grid.units))).

do_round(Grid) ->
    move_or_attack(gb_trees:iterator(Grid#grid.units), Grid).

move_or_attack(Iter, Grid) ->

    %% ?LOG("Grid:~n~s~n", [grid_to_string(Grid)]),

    %% On each unit's turn, it tries to (1) MOVE into range (if it
    %% isn't already) and then (2) ATTACK (if it is in range).

    case gb_trees:next(Iter) of
	none ->
	    %% No more units to move in this round.
	    Grid;

	{Pos, {Type, _, _}, NextIter} ->

            %% ?LOG("~n=======[ ~p (~p) ]========~n", [Pos, Type]),

            %% Note that we cannot use the HP received from the
            %% iterator since the HP may have been modified due to
            %% attacks by other units.

	    case gb_trees:lookup(Pos, Grid#grid.units) of
		none ->
                    %% Unit was killed before its turn.
		    %% ?LOG("~p unit at ~p was killed before its turn.~n", [Type, Pos]),
                    move_or_attack(NextIter, Grid);

		{value, _} ->
		    Enemies = get_enemies_of(Type, Grid),

		    case find_path(Pos, Enemies, Grid) of
			no_enemies ->
			    %% There are no enemies left. Attacking
			    %% team wins.
			    %% ?LOG("No enemies left, winner is ~p~n", [Type]),
			    {winner, Type, Grid};

			no_path ->
			    %% There is no path from this unit to any enemy.
			    %% The unit cannot move, and ends it's turn.
			    %% ?LOG("~p unit at ~p can not reach any enemy~n", [Type, Pos]),
			    move_or_attack(NextIter, Grid);

			NewPos ->
			    GridAfterMove = apply_move(Pos, NewPos, Grid),
			    GridAfterCombat = combat(NewPos, GridAfterMove),
			    move_or_attack(NextIter, GridAfterCombat)
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


    %% ?LOG("Enemies in range of ~p: ~p~n", [Pos, EnemiesInRange]),

    case EnemiesInRange of
	[] ->
	    %% ?LOG("No enemies in range, combat can not be done.~n", []),
	    Grid;
	_ ->
	    MinHP = lists:min(lists:map(fun({_, {_, HP, _}}) ->
						HP
					end, EnemiesInRange)),
	    [{EnemyPos, {EnemyType, EnemyHP, EnemyAP}}|_] =
		lists:filter(fun({_, {_, HP, _}}) -> HP == MinHP end, EnemiesInRange),


	    NewEnemyHP = EnemyHP - AttackPower,

	    %% ?LOG("Attacked enemy at ~p, new enemy hp = ~p~n",
	    %% 	 [EnemyPos, NewEnemyHP]),

	    %% Delete enemy unit if dead, otherwise update its HP
	    NewUnits =
		if NewEnemyHP =< 0 ->
                        io:format("~w~n",
                                  [{{dead, EnemyType}, {attacker, Pos}, {pos, EnemyPos}}]),

                        trace_output(Grid,
                                     {{dead, EnemyType}, {pos, EnemyPos}}),
			gb_trees:delete(EnemyPos, Grid#grid.units);
		   true ->
			gb_trees:update(EnemyPos, {EnemyType, NewEnemyHP, EnemyAP}, Grid#grid.units)
		end,

	    Grid#grid{units = NewUnits}
    end.

%% Apply the move; moving Pos to NewPos, and recalculating the grid
%% and iterator.
apply_move(Pos, Pos, Grid) ->
    Grid;
apply_move(Pos, NewPos, Grid) ->
    Val = gb_trees:get(Pos, Grid#grid.units),
    NewUnits = gb_trees:insert(NewPos, Val,
			       gb_trees:delete(Pos, Grid#grid.units)),
    Grid#grid{units = NewUnits}.

get_enemies_of(Type, Grid) ->
    lists:filtermap(
      fun({EnemyPos, {EnemyType, _, _}}) ->
	      if (EnemyType =/= Type) -> {true, EnemyPos};
		 true -> false
	      end
      end, gb_trees:to_list(Grid#grid.units)).

adjacent({Y, X}) ->
    [{Y - 1, X},
     {Y, X - 1},
     {Y, X + 1},
     {Y + 1, X}].

is_open(Pos, Grid) ->
    (not sets:is_element(Pos, Grid#grid.walls))
	and (not gb_trees:is_defined(Pos, Grid#grid.units)).

manhattan_dist({Y1, X1}, {Y2, X2}) ->
    abs(X1 - X2) + abs(Y1 - Y2).

open_and_adjacent(Pos, Grid) ->
    lists:filter(
      fun(AdjPos) ->
	      is_open(AdjPos, Grid)
      end, adjacent(Pos)).

-record(search, {
	  enemies,
	  open,
	  closed = sets:new(),
	  grid,
	  nearest = sets:new(),
	  best = inf
	 }).

get_default_search_params(StartPos, Enemies, Grid) ->
    #search {
       open = gb_sets:singleton({0, StartPos}),
       closed = sets:new(),
       enemies = Enemies,
       grid = Grid,
       nearest = sets:new(),
       best = inf
      }.

find_path(_, Enemies, _Grid) when length(Enemies) == 0 ->
    no_enemies;
find_path(StartPos, Enemies, Grid) ->
    %% ?LOG("Finding shortest path from ~p to enemies ~p~n", [StartPos, Enemies]),

    Search = get_default_search_params(StartPos, Enemies, Grid),
    S0 = find_path(Search),

    case lists:sort(sets:to_list(S0#search.nearest)) of
	[] -> no_path;
	[Chosen|_] = AllEnemyAdj ->
	    {ChosenPos, _} = Chosen,

	    %%?LOG("Start pos = ~p~n", [StartPos]),
	    %%?LOG("Chosen = ~p~n", [ChosenPos]),

	    if StartPos =:= ChosenPos ->
		    %% The start position was one of the adjacent
		    %% squares, use it.
		    StartPos;
	       true ->
		    %% Now, we need to find out which square to start from.
		    Adj = open_and_adjacent(StartPos, Grid),

		    %%?LOG("Choosing enemy-adjacent square ~p from ~p~n", [Chosen, AllEnemyAdj]),
		    %%?LOG("Determining which square to start from: ~p~n", [Adj]),

		    case Adj of
			[X] -> X;
			[_|_] ->
			    %% We have more than one square to choose from.
			    %% Make a search from each of the squares and see
			    %% which one to use.

			    %% All routes from any adjacent square to the choosen
			    %% enemy-adjacent square
			    Routes =
				lists:filtermap(
				  fun(Start) ->
					  %% ?LOG("Finding route from ~p to ~p~n", [Start, ChosenPos]),
					  Params = get_default_search_params(
						     Start, [ChosenPos], Grid),
					  P0 = find_path(Params),
					  Ns = sets:to_list(P0#search.nearest),
					  %% ?LOG("Routes found: ~p~n", [Ns]),
					  case Ns of
					      [] -> false;
					      [{_, Dist}|_] -> {true, {Dist, Start}}
					  end
				  end, Adj),

			    %% ?LOG("All closest routes: ~p~n", [Routes]),
			    %% case Routes of
			    %% 	[] ->
			    %% 	    ?LOG("No routes, this should not happen. Grid ~n~s~n",
			    %% 		 [grid_to_string(Grid)]);
			    %% 	_ ->
			    %% 	    ok
			    %% end,

			    %% Sort them on distance, and pick the best ones.
			    SortedRoutes = lists:sort(Routes),
			    [{Best, _}|_] = SortedRoutes,


			    BestRoutes = lists:takewhile(fun({X, _}) ->
								 X == Best
							 end, SortedRoutes),

			    %% Take the best route, break ties in reading order.
			    [{_, BestStartPos}|_] = lists:sort(BestRoutes),

			    %% ?LOG("Chosing closest route in reading order: ~p~n", [BestStartPos]),
			    BestStartPos
		    end
	    end
    end.

find_path(Search) ->
    Open = Search#search.open,
    case gb_sets:size(Open) of
	0 ->
	    %% ?LOG("Open set is empty, search is finished.~n", []),
	    Search;
	_ ->
	    {{Dist, Elem}, Open0} = gb_sets:take_smallest(Open),

	    Best = Search#search.best,
	    Search00 = Search#search{open = Open0},

	    IsAdjToEnemy =
		lists:any(fun(P) -> manhattan_dist(P, Elem) =< 1 end,
			  Search00#search.enemies),

	    %% ?LOG("Searching node ~p at dist ~p (is adjacent: ~p)~n", [Elem, Dist, IsAdjToEnemy]),

	    if IsAdjToEnemy and (Dist > Best) ->
		    %% ?LOG("Pruning search when finding ~p at dist ~p~n", [Elem, Dist]),
		    Search00;
	       true ->
		    Search0 =
			if IsAdjToEnemy and (Dist < Best) ->
				%% ?LOG("Found better solution ~p at dist ~p~n", [Elem, Dist]),
				Search00#search{best = Dist, nearest = sets:from_list([{Elem, Dist}])};
			   IsAdjToEnemy and (Dist == Best) ->
				%% ?LOG("Found equal solution ~p at dist ~p~n", [Elem, Dist]),
				Nearest = Search00#search.nearest,
				Nearest0 = sets:add_element({Elem, Dist}, Nearest),
				Search00#search{nearest = Nearest0};
			   true ->
				Search00
			end,

		    Adjacent = open_and_adjacent(Elem, Search0#search.grid),

		    Search1 =
			lists:foldl(
			  fun(Adj, Acc) ->
				  case sets:is_element(Adj, Acc#search.closed) of
				      true -> Acc;
				      false ->
					  case gb_sets:is_element(Adj, Acc#search.open) of
					      true -> Acc;
					      false ->
						  Open1 = gb_sets:add_element({Dist + 1, Adj}, Acc#search.open),
						  Acc#search{open = Open1}
					  end
				  end
			  end, Search0, Adjacent),

		    Search2 =
			Search1#search{closed = sets:add_element(Elem, Search1#search.closed)},



		    find_path(Search2)
	    end
    end.

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

find_path1_test() ->
    Bin = <<"#######\n",
	    "#E..G.#\n",
	    "#...#.#\n",
	    "#.G.#G#\n",
	    "#######\n">>,
    Grid = parse_grid(Bin, 0),
    Enemies = get_enemies_of('E', Grid),
    ?assertEqual({1, 2}, find_path({1, 1}, Enemies, Grid)).

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
    Enemies = get_enemies_of('G', Grid),
    ?assertEqual({2, 4}, find_path({2, 4}, Enemies, Grid)).

move3_test() ->
    Bin = <<"#######\n",
	    "#..G..#\n",
	    "#...G.#\n",
	    "#.#G#G#\n",
	    "#...#E#\n",
	    "#.....#\n",
	    "#######\n">>,
    Grid = parse_grid(Bin, 0),
    Enemies = get_enemies_of('G', Grid),
    ?assertEqual({1, 2}, find_path({1, 3}, Enemies, Grid)).

ex_helper(Binary) ->
    Grid = parse_grid(Binary, 3),
    do_battle_until_death(1, Grid).

ex0_test() ->
    Bin = <<"#######\n",
	    "#.G...#\n",
	    "#...EG#\n",
	    "#.#.#G#\n",
	    "#..G#E#\n",
	    "#.....#\n",
	    "#######\n">>,
    ?assertEqual(27730, ex_helper(Bin)).

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
