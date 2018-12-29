%%% @author Jesper Eskilson <jesper@eskilson.se>
%%% @copyright (C) 2018
%%% @doc
%%%
%%% A* algorithm implemented in Erlang. Adapted from the pseudocode in
%%% https://en.wikipedia.org/wiki/A*_search_algorithm.
%%%
%%% @end

-module(astar).
-export([a_star/3]).
-include_lib("eunit/include/eunit.hrl").

-record(astar,
	{
	  start,
	  goal,
	  closed_set,
	  open_set,
	  came_from,
	  gscore,
	  fscore,
	  callback
	}).

a_star(Start, Goal, Fun) ->
    AStar =
	#astar{
	   start = Start,
	   goal = Goal,
	   open_set = sets:from_list([Start]),
	   closed_set = sets:new(),
	   came_from = maps:new(),
	   gscore = #{Start => 0},
	   fscore = #{Start => Fun({cost, Start, Goal})},

	   %% Callback function is used to obtain user-specific
	   %% parameters during search. It is called in three
	   %% different ways:
	   %%
	   %% 1. Fun({neighbors, Current}) to return the nodes adjacent
	   %% to Current.
	   %%
	   %% 2. Fun({cost, Neighbor, Goal}) to return the estimated
	   %% cost of going from Neighbor to Goal.
	   %%
	   %% 3. Fun({dist, Current, Neighbor}) to return the actual
	   %% distance from the Current node to one of its neighbors.
	   callback = Fun
	  },

    a_star(AStar).

a_star(AStar) ->
    case sets:size(AStar#astar.open_set) of
	0 ->
	    %% no more nodes to discover
	    {error, could_not_find_goal};
	_ ->
	    Current = get_node_with_lowest_fscore(AStar),

	    if Current == AStar#astar.goal ->
		    {ok, reconstruct_path(AStar, Current)};
	       true ->
		    %% io:format("Current node: ~p~n", [Current]),
		    a_star_recurse(AStar, Current)
	    end
    end.

a_star_recurse(AStar, Current) ->
    AStar0 = move_from_open_to_closed(AStar, Current),

    Fun = AStar#astar.callback,
    Neighbors = Fun({neighbors, Current}),

    AStarNext =
	lists:foldl(
	  fun(Neighbor, AStarIn) ->
		  ClosedSet = AStarIn#astar.closed_set,
		  OpenSet = AStar#astar.open_set,
		  GScore = AStar#astar.gscore,

		  NewGScore = maps:get(Current, GScore, inf) +
		      Fun({dist, Neighbor, Current}),
		  NeighborGScore = maps:get(Neighbor, GScore, inf),
		  NewGScoreIsBetter = NewGScore < NeighborGScore,

		  InClosed = sets:is_element(Neighbor, ClosedSet),
		  InOpen = sets:is_element(Neighbor, OpenSet),

		  %% There are four distinct cases, depending on whether
		  %% the neighbor has been seen or not, and if any new
		  %% path to it has a better score or not.

		  case {InClosed, InOpen, NewGScoreIsBetter} of
		      {true, _, _} ->
			  %% 1. Neighbor is in closed set (we've already
			  %% evaluated it).
			  AStarIn;

		      {false, false, _} ->
			  %% 2. Neighbor has not been seen before, add it
			  %% to the open set and record the current path
			  %% to it as the best one.
			  AStar1 = AStarIn#astar{
				     open_set = sets:add_element(Neighbor, OpenSet)},
			  record_best_path(AStar1, Current, Neighbor, NewGScore);

		      {false, true, true} ->
			  %% 3. Neighbor has been seen before but this new
			  %% path is better.
			  record_best_path(AStarIn, Current,
					   Neighbor, NewGScore);

		      {false, true, false} ->
			  %% 4. Neighbor has been seen before, but the old
			  %% path was better, so ignore the new one.
			  AStarIn
		  end
	  end, AStar0, Neighbors),

    a_star(AStarNext).

%%% ============================================================
%%% Helper functions
%%% ============================================================

reconstruct_path(AStar, Current) ->
    lists:reverse(reconstruct_path0(AStar, Current)).

reconstruct_path0(AStar, Current) ->
    case maps:is_key(Current, AStar#astar.came_from) of
	true ->
	    Parent = maps:get(Current, AStar#astar.came_from),
	    [Current|reconstruct_path0(AStar, Parent)];
	false ->
	    [Current]
    end.

record_best_path(#astar{came_from = CameFrom,
			goal = Goal,
			gscore = GScore,
			fscore = FScore,
			callback = Fun} = AStar, Current, Neighbor, NewGScore) ->
    AStar#astar{came_from = maps:put(Neighbor, Current, CameFrom),
		gscore = maps:put(Neighbor, NewGScore, GScore),
		fscore = maps:put(Neighbor, NewGScore +
				      Fun({cost, Neighbor, Goal}), FScore)}.

get_node_with_lowest_fscore(AStar) ->
    {Node, _} =
	sets:fold(fun(Node, {undef, _}) ->
			  {Node, maps:get(Node, AStar#astar.fscore, inf)};
		     (Node, {_, Min} = AccIn) ->
			  case maps:get(Node, AStar#astar.fscore, inf) of
			      NodeFScore when NodeFScore < Min ->
				  {Node, NodeFScore};
			      _ ->
				  AccIn
			  end
		  end, {undef, inf}, AStar#astar.open_set),
    Node.

move_from_open_to_closed(#astar{open_set = OpenSet,
				closed_set = ClosedSet} = AStar, Element) ->
    AStar#astar{open_set = sets:del_element(Element, OpenSet),
		closed_set = sets:add_element(Element, ClosedSet)}.

%%% ============================================================
%%% Tests
%%% ============================================================

get_node_with_lowest_fscore_test() ->
    AStar = #astar{
	       open_set = sets:from_list([a, b, c, d]),
	       fscore = #{ b => 1, c => 0, a => 10, d => 3 }
	      },
    ?assertEqual(c, get_node_with_lowest_fscore(AStar)).

reconstruct_path_test() ->
    AStar = #astar{came_from = #{e => d, d => c, c => b, b => a}},
    ?assertEqual([a, b, c, d, e], reconstruct_path(AStar, e)).

record_best_path_test() ->
    AStar = #astar{
	       came_from = #{},
	       goal = e,
	       callback = fun({cost, _N, _G}) -> 42 end,
	       gscore = #{},
	       fscore = #{}
	      },
    AStar0 = record_best_path(AStar, b, c, 5),
    ?assertEqual(#{c => b}, AStar0#astar.came_from),
    ?assertEqual(#{c => 5}, AStar0#astar.gscore),
    ?assertEqual(#{c => 47}, AStar0#astar.fscore).

move_from_open_to_closed_test() ->
    AStar = #astar{
	       open_set = sets:from_list([a, b, c]),
	       closed_set = sets:from_list([d, e, f])
	      },
    AStar0 = move_from_open_to_closed(AStar, b),
    ?assertEqual([a, c], lists:sort(sets:to_list(AStar0#astar.open_set))),
    ?assertEqual([b, d, e, f], lists:sort(sets:to_list(AStar0#astar.closed_set))).

adjacent({X, Y}, {MinX, MaxX, MinY, MaxY}) ->
    [{Xa, Ya} ||
	Xa <- lists:seq(X - 1, X + 1),
	Ya <- lists:seq(Y - 1, Y + 1),
	Xa >= MinX,
	Xa =< MaxX,
	Ya >= MinY,
	Ya =< MaxY,
	{Xa, Ya} /= {X, Y}].

adjacent_test() ->
    ?assertEqual([{0, 1}, {1, 0}, {1, 1}], adjacent({0, 0}, {0, 9, 0, 9})).

a_star_test() ->
    Grid = <<"S....#....",
	     ".....#....",
	     "..##.#.#..",
	     "..##...#..",
	     ".......##.",
	     ".......#..",
	     "########..",
	     ".........#",
	     "....######",
	     ".........G">>,
    Size = 10,
    Obstacles = sets:from_list(
		  [pos_to_coord(Pos, Size)
		   || Pos <- binary:matches(Grid, <<"#">>)]),

    Start = {0, 0},
    Goal = {9, 9},

    {ok, Path} =
	a_star(Start, Goal,
	       fun({neighbors, Current}) ->
		       Neighbors = adjacent(Current, {0, Size-1, 0, Size-1}),
		       lists:filter(fun(N) ->
					    not sets:is_element(N, Obstacles)
				    end, Neighbors);
		  ({dist, Neighbor, Current}) ->
		       distance(Neighbor, Current);
		  ({cost, Neighbor, G}) ->
		       distance(Neighbor, G)
	       end),

    X = [[pos_to_str({X,Y}, Size, Path, Grid) ||
     	     X <- lists:seq(0, Size - 1)] ++ "\n" ||
     	    Y <- lists:seq(0, Size - 1)],

    io:format("Path:~n~s~n", [X]),
    ?assertEqual(25, length(Path)).

pos_to_str({X, Y}, Size, Path, Grid) ->
    case lists:member({X,Y}, Path) of
	true ->
	    $*;
	false ->
	    binary:at(Grid, Y * Size + X)
    end.

distance({X0, Y0}, {X1, Y1}) ->
    abs(X0 - X1) + abs(Y0 - Y1).

pos_to_coord({Start, _}, W) ->
    {Start rem W, Start div W}.
