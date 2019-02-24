%%% @author Jesper Eskilson <jesper.eskilson@klarna.com>
%%% @copyright (C) 2019, Jesper Eskilson
%%% @doc
%%%
%%% @end
%%% Created :  2 Feb 2019 by Jesper Eskilson <jesper.eskilson@klarna.com>

-module(puzzle15).
-export([main/0, part1/0, part2/0]).

-include_lib("eunit/include/eunit.hrl").

-define(UNIT_HP, 200).
-define(GOBLIN_ATTACK_POWER, 3).
-define(DEFAULT_ELF_ATTACK_POWER, 3).

-record(grid, {
               walls = sets:new(),
               units = maps:new(),
               width = 0,
               height = 0,
               elf_attack_power = ?DEFAULT_ELF_ATTACK_POWER,
               elf_deaths = 0
	 }).

main() ->
    {{part1, part1()},
     {part2, part2()}}.

part1() ->
    Grid = read_grid("input.txt", ?DEFAULT_ELF_ATTACK_POWER),
    do_battle_until_death(1, Grid, true).

part2() ->
    do_battle_with_elf_boost(?DEFAULT_ELF_ATTACK_POWER).

do_battle_with_elf_boost(ElfAttackPower) ->
    Grid = read_grid("input.txt", ElfAttackPower),
    do_battle_until_death(1, Grid, false).

do_battle_until_death(N, Grid, AllowElfDeaths) ->
    case do_round(Grid) of
        {winner, _, FinalGrid} ->
            FullRounds = N - 1,
	    SumHP = sum_hp(FinalGrid),
	    FullRounds * SumHP;
        NewGrid when AllowElfDeaths or (NewGrid#grid.elf_deaths == 0) ->
            do_battle_until_death(N + 1, NewGrid, AllowElfDeaths);
        _ ->
            do_battle_with_elf_boost(Grid#grid.elf_attack_power + 1)
    end.

sum_hp(Grid) ->
    lists:sum(lists:map(fun({_, {_, HP, _, _}}) -> HP end,
                        gb_trees:to_list(Grid#grid.units))).

do_round(Grid) ->
    round_per_unit(gb_trees:iterator(Grid#grid.units), Grid).

%% Execute move/combat actions for each unit.
round_per_unit(Iter, Grid) ->
    case gb_trees:next(Iter) of
	none ->
	    %% No more units to move in this round.
	    Grid;

	{Pos, {Type, _, _, UnitId}, NextIter} ->

            %% Note that we cannot use the HP received from the
            %% iterator since the HP may have been modified due to
            %% attacks by other units.

	    case gb_trees:lookup(Pos, Grid#grid.units) of
		{value, {Type, _, _, UnitId}} ->
		    Enemies = get_enemies_of(Type, Grid),

		    case find_path(Pos, Enemies, Grid) of
			no_enemies ->
			    %% There are no enemies left. Attacking
			    %% team wins.
			    {winner, Type, Grid};

			no_path ->
			    %% There is no path from this unit to any
			    %% enemy. The unit cannot move, and ends
			    %% it's turn.
			    round_per_unit(NextIter, Grid);

			NewPos ->
			    GridAfterMove = apply_move(Pos, NewPos, Grid),
			    GridAfterCombat = combat(NewPos, GridAfterMove),
			    round_per_unit(NextIter, GridAfterCombat)
		    end;
                _ ->
                    %% Unit was killed before its turn.
                    round_per_unit(NextIter, Grid)
            end
    end.


combat(Pos, Grid) ->
    %% At beginning of combat, the unit considers all enemies in
    %% range, not just the one we happened to move towards.

    {Type, _HP, AttackPower, _} = gb_trees:get(Pos, Grid#grid.units),

    EnemiesInRange =
	lists:sort(
	  lists:filter(fun({EnemyPos, {EnemyType, _, _, _}}) ->
			       (EnemyType =/= Type)
				   and (manhattan_dist(Pos, EnemyPos) == 1)
		       end, gb_trees:to_list(Grid#grid.units))),


    %% ?LOG("Enemies in range of ~p: ~p~n", [Pos, EnemiesInRange]),

    case EnemiesInRange of
	[] ->
	    %% ?LOG("No enemies in range, combat can not be done.~n", []),
	    Grid;
	_ ->
	    MinHP = lists:min(lists:map(fun({_, {_, HP, _, _}}) ->
						HP
					end, EnemiesInRange)),
	    [{EnemyPos, {EnemyType, EnemyHP, EnemyAP, EnemyId}}|_] =
		lists:filter(fun({_, {_, HP, _, _}}) -> HP == MinHP end, EnemiesInRange),


	    NewEnemyHP = EnemyHP - AttackPower,

	    %% Delete enemy unit if dead, otherwise update its HP
	    NewUnits =
		if NewEnemyHP =< 0 ->
			gb_trees:delete(EnemyPos, Grid#grid.units);
		   true ->
			gb_trees:update(EnemyPos, {EnemyType, NewEnemyHP, EnemyAP, EnemyId}, Grid#grid.units)
		end,

            ElfDeath = if (NewEnemyHP =< 0) and (EnemyType =:= 'E') -> 1;
                          true -> 0
                       end,
           
	    Grid#grid{units = NewUnits,
                      elf_deaths = Grid#grid.elf_deaths + ElfDeath}
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
      fun({EnemyPos, {EnemyType, _, _, _}}) ->
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

    Search = get_default_search_params(StartPos, Enemies, Grid),
    S0 = find_path(Search),

    case lists:sort(sets:to_list(S0#search.nearest)) of
	[] -> no_path;
	[Chosen|_] ->
	    {ChosenPos, _} = Chosen,

	    if StartPos =:= ChosenPos ->
		    %% The start position was one of the adjacent
		    %% squares, use it.
		    StartPos;
	       true ->
		    %% Now, we need to find out which square to start from.
		    Adj = open_and_adjacent(StartPos, Grid),

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
					  Params = get_default_search_params(
						     Start, [ChosenPos], Grid),
					  P0 = find_path(Params),
					  Ns = sets:to_list(P0#search.nearest),
					  case Ns of
					      [] -> false;
					      [{_, Dist}|_] -> {true, {Dist, Start}}
					  end
				  end, Adj),

			    %% Sort them on distance, and pick the best ones.
			    SortedRoutes = lists:sort(Routes),
			    [{Best, _}|_] = SortedRoutes,


			    BestRoutes = lists:takewhile(fun({X, _}) ->
								 X == Best
							 end, SortedRoutes),

			    %% Take the best route, break ties in reading order.
			    [{_, BestStartPos}|_] = lists:sort(BestRoutes),
			    BestStartPos
		    end
	    end
    end.

find_path(Search) ->
    Open = Search#search.open,
    case gb_sets:size(Open) of
	0 ->
	    Search;
	_ ->
	    {{Dist, Elem}, Open0} = gb_sets:take_smallest(Open),

	    Best = Search#search.best,
	    Search00 = Search#search{open = Open0},

	    IsAdjToEnemy =
		lists:any(fun(P) -> manhattan_dist(P, Elem) =< 1 end,
			  Search00#search.enemies),

	    if IsAdjToEnemy and (Dist > Best) ->
		    Search00;
	       true ->
		    Search0 =
			if IsAdjToEnemy and (Dist < Best) ->
				Search00#search{best = Dist, 
                                                nearest = sets:from_list([{Elem, Dist}])};
			   IsAdjToEnemy and (Dist == Best) ->
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
		{value, {Type, HP, _, _}} when HP > 0 ->
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
              {Type, HP, _, _} = gb_trees:get(Unit, Grid#grid.units),
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
				     {{Y, X}, 
                                      {C, ?UNIT_HP, 
                                       unit_attack_power(C, ElfAttackPower),
                                       %% Use as unique key to
                                       %% distinguish different units
                                       erlang:timestamp()}}
			     end, Units))),

       walls = sets:from_list(
		 lists:map(fun({Y, X, _}) ->
				   {Y, X}
			   end, Walls)),

       width = length(First),
       height = length(Lines),
       elf_attack_power = ElfAttackPower
      }.


read_grid(Filename, ElfAttackPower) ->
    {ok, Binary} = file:read_file(Filename),
    parse_grid(Binary, ElfAttackPower).

%%% Utilities

unit_attack_power('E', ElfAttackPower) -> ElfAttackPower;
unit_attack_power('G', _) -> ?GOBLIN_ATTACK_POWER.

%%% Tests

pretty_print_test() ->
    Bin = <<"#######\n",
	    "#.G...#\n",
	    "#...EG#\n",
	    "#.#.#G#\n",
	    "#..G#E#\n",
	    "#.....#\n",
	    "#######\n">>,
    io:format("~s~n", [grid_to_string(parse_grid(Bin, 3))]).

ex_helper(Binary) ->
    Grid = parse_grid(Binary, 3),
    do_battle_until_death(1, Grid, true).

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
