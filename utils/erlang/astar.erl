%%% @author  <jespe@LAPTOP-P6HKA27J>
%%% @copyright (C) 2018, 
%%% @doc
%%% A* algorithm implemented in Erlang.
%%% @end
%%% Created : 28 Dec 2018 by  <jespe@LAPTOP-P6HKA27J>

-module(astar).
-compile([export_all]).
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
	  cost_heuristic,
	  get_neighbors
	}).

a_star_move_from_closed_to_open(
  #{open_set := OpenSet, closed_set := ClosedSet} = AStar, Element) ->
    AStar#astar{open_set = sets:del_element(Element, OpenSet),
		closed_set = sets:add_element(Element, ClosedSet)}.

a_star(Start, Goal, CostHeuristicFun, GetNeighborsFun) ->
    AStar = 
	#astar{
	   open_set = sets:new(),
	   closed_set = sets:new(),
	   came_from = maps:new(),
	   gscore = #{Start => 0},
	   fscore = #{Start => CostHeuristicFun(Start, Goal)},
	   cost_heuristic = CostHeuristicFun,
	   get_neighbors = GetNeighborsFun
	  },

    a_star(AStar).

a_star(AStar) ->
    case maps:size(AStar#astar.open_set) of
	0 ->
	    %% no more nodes to discover
	    {error, could_not_find_goal};
	_ ->
	    Current = get_node_with_lowest_fscore(AStar),
	    
	    if Current == AStar#astar.goal ->
		    reconstruct_path(AStar, Current);
	       true ->
		    a_star_recurse(AStar, Current)
	    end
    end.

a_star_recurse(AStar, Current) ->
    AStar0 = a_star_move_from_closed_to_open(AStar, Current),
    GetNeighborsFun = AStar#astar.get_neighbors,    
    Neighbors = GetNeighborsFun(Current),
    
    lists:foldl(
      fun(Neighbor, AStarIn) ->
	      ClosedSet = AStarIn#astar.closed_set,
	      GScore = AStarIn#astar.gscore,
	      
	      case sets:is_member(Neighbor, ClosedSet) of
		  true ->
		      %% Neighbor is in closed set, ignore it.
		      AStarIn;
		  _ ->
		      TentativeGScore = maps:get(Current, GScore, inf)
		      %%
	      end
      end, AStar, Neighbors).

reconstruct_path(AStar, Current) ->    
    ok.

get_node_with_lowest_fscore(AStar) ->
    {Node, _} = 
	sets:fold(fun(Node, {undef, _} = AccIn) ->
			  {Node, maps:get(Node, AStar#astar.fscore, inf)};
		     (Node, {CurrMinNode, Min} = AccIn) ->
			  case maps:get(Node, AStar#astar.fscore, inf) of
			      NodeFScore when NodeFScore < Min ->
				  {Node, NodeFScore};
			      _ ->
				  AccIn
			  end
		  end, {undef, inf}, AStar#astar.open_set),
    Node.

get_node_with_lowest_fscore_test() ->
    AStar = #astar{
	       open_set = sets:from_list([a, b, c, d]),
	       fscore = #{ b => 1, c => 0, a => 10, d => 3 }
	      },
    ?assertEqual(c, get_node_with_lowest_fscore(AStar)).

