%%% @author Jesper Eskilson <jesper.eskilson@klarna.com>
%%% @copyright (C) 2019, Jesper Eskilson
%%% @doc
%%%
%%% @end
%%% Created :  7 Jan 2019 by Jesper Eskilson <jesper.eskilson@klarna.com>

-module(puzzle13).
-compile([export_all]).

start() ->
    start(50, 1364, 25).

start(Depth, Fav, Size) ->  
    Start = {1, 1},
    Goal = {31, 39},
    Path = 
        astar2:astar(Start, Goal,
                     fun(Node) -> cost(Node, Goal) end,
                     fun(Curr) -> neighbors(Curr, Fav) end,
                     fun distance/2),

    Reachable = search_to_depth(Start, Depth, Fav),

    S = [[pos_to_str({X,Y}, Start, Reachable, Fav) 
          || X <- lists:seq(0, Size)] ++ "\n"
         || Y <- lists:seq(0, Size)],
    io:format("~s~n", [S]),

    {{part1, length(Path) - 1},
     {part2, sets:size(search_to_depth(Start, Depth, Fav))}}.

pos_to_str(Pos, Start, _Reachable, _Fav) when Pos == Start -> "S";
pos_to_str(Pos, _Start, Reachable, Fav) ->
    case is_wall(Pos, Fav) of
        true -> "#";
        false -> 
            case sets:is_element(Pos, Reachable) of
                true ->
                    "X";
                false ->
                    " "
            end
    end.

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
