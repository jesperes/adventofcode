-module(aoc2016_day13).

-include_lib("eunit/include/eunit.hrl").

main_test_() ->
  Depth = 50,
  Fav = 1364,
  Start = {1, 1},
  Goal = {31, 39},

  [ {"Part 1",
     fun() ->
         {_, Path} =
           astar2:astar(Start, Goal,
                        fun(Node) -> cost(Node, Goal) end,
                        fun(Curr) -> neighbors(Curr, Fav) end,
                        fun distance/2),
         ?assertEqual(86, length(Path) - 1)
     end}
  , {"Part 2",
     fun() ->
         Reachable = search_to_depth(Start, Depth, Fav),
         ?assertEqual(127, sets:size(Reachable))
     end}
  ].

bitcount(V)    -> bitcount(V, 0).
bitcount(0, C) -> C;
bitcount(V, C) -> bitcount(V band (V - 1), C + 1).

is_wall({X, Y}, Fav) ->
  bitcount((X*X + 3*X + 2*X*Y + Y + Y*Y + Fav)) rem 2 == 1.

%%% A* search algorithm callbacks

distance(_, _) -> 1.
cost({X0,Y0}, {X1,Y1}) -> abs(X0 - X1) + abs(Y0 - Y1).
neighbors({X, Y}, Fav) ->
  lists:filter(
    fun({X0, Y0} = Pos) ->
        (X0 >= 0) and (Y0 >= 0) and (not is_wall(Pos, Fav))
    end,
    [{X - 1, Y},
     {X + 1, Y},
     {X, Y + 1},
     {X, Y - 1}]).

%%% Part 2: enumerate all positions reachable using max 50 steps.  Use
%%% a depth-first search, but keep track of the depth at which each
%%% node was found so we can revisit nodes when seen at an earlier
%%% depth.

search_to_depth(Start, Depth, Fav) ->
  Visited = search_to_depth(Start, 1, Depth, Fav, #{Start => 0}),
  sets:from_list(maps:keys(Visited)).

search_to_depth(_, CurrDepth, MaxDepth, _Fav, Visited) when CurrDepth > MaxDepth ->
  Visited;
search_to_depth(Curr, CurrDepth, MaxDepth, Fav, Visited) ->
  lists:foldl(
    fun(Nbr, VisitedIn) ->
        case maps:get(Nbr, VisitedIn, inf) of
          NbrDepth when NbrDepth > CurrDepth ->
            search_to_depth(Nbr, CurrDepth + 1, MaxDepth, Fav,
                            VisitedIn#{Nbr => CurrDepth});
          _ ->
            VisitedIn
        end
    end, Visited, neighbors(Curr, Fav)).
