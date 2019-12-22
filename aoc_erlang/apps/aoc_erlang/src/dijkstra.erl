-module(dijkstra).

-include_lib("eunit/include/eunit.hrl").

-export([ dijkstra/4
        , shortest_dist/2
        , shortest_path/2
        ]).

-record(state,
        { source
        , graph
        , closed
        , frontier
        , distances
        , parents
        , nbrfun
        , endfun
        }).

shortest_dist(#state{distances = Map}, Node) ->
  maps:get(Node, Map).

shortest_path(State, Node) ->
  shortest_path(State, Node, [Node]).

shortest_path(#state{parents = Parents, closed = Closed} = State, Node, Path) ->
  ?assert(maps:is_key(Node, Closed)),
  case maps:get(Node, Parents, undef) of
    undef -> Path;
    Parent -> shortest_path(State, Parent, [Parent|Path])
  end.

%%
%% Apply dijkstra's algorithm to Graph, beginning at Source.
%%
%% @param Graph   The graph to search.
%% @param Source  Start node
%% @param Fun     Neighbor callback function. Returns a list of
%%                {Dist,Node} tuples for each of the node's neighbors.
%% @param EndFun  Returns true iff the given node is the end node, and
%%                the search should stop.
%% @returns       A opaque search state, which can be passed to e.g.
%%                shortest_path/2 to find the shortest path to a node.
%%
-spec dijkstra(Graph :: term(),
               Source :: term(),
               Fun :: fun((Node :: term(), Graph :: term()) ->
                             list({NbrDist :: integer(),
                                   NbrNode :: term()})),
               EndFun :: fun((Node :: term(), Graph :: term()) ->
                                boolean())) ->
                  {finished, Result :: #state{}} |
                  {found, Result :: #state{}}.
dijkstra(Graph, Source, Fun, EndFun) ->
  State =
    #state{ source = Source
          , graph = Graph
          , closed = #{}
          , frontier = gb_sets:from_list([{0, Source}])
          , distances = #{Source => 0}
          , parents = #{}
          , nbrfun = Fun
          , endfun = EndFun
          },

  dijkstra0(State).

dijkstra0(#state{ frontier = Frontier
                , nbrfun = Fun
                , endfun = EndFun
                , graph = Graph
                } = State) ->
  %% ?debugFmt("~nFrontier: ~p", [gb_sets:to_list(Frontier)]),

  case gb_sets:is_empty(Frontier) of
    true ->
      %% We have exhausted the search, no more nodes
      %% to evaluate.
      {finished, State};
    false ->
      %% Pick the node from the frontier with the shortest distance
      %% from origin.
      {{NodeDist, Node}, State0} = take_frontier(State),

      case EndFun(Node, Graph) of
        true ->
          %% We have found our target
          {found, State};
        false ->
          %% Find neighbors
          Neighbors = Fun(Node, Graph),

          ?debugFmt("~nNeighbors(~p) = ~p", [Node, Neighbors]),

          %% Mark node as evaluated (this means that we know the shortest
          %% path to this node).
          State1 = add_to_closed(Node, State0),

          %% Fold over the neighbors, and repeat.
          dijkstra0(
            lists:foldl(
              fun({NbrDist, NbrNode}, Acc) ->
                  case is_closed(NbrNode, Acc) of
                    true ->
                      %% Neighbor is in closed set, i.e. already evaluated
                      Acc;
                    false ->
                      case is_in_frontier(NbrNode, Acc) of
                        true ->
                          NewNbrDist = NodeDist + NbrDist,
                          OldNbrDist = maps:get(NbrNode, Acc#state.distances),

                          if NewNbrDist < OldNbrDist ->
                              update_distance(Node, NbrNode, NewNbrDist, Acc);
                             true ->
                              Acc
                          end;
                        false ->
                          %% New node, add it to the frontier
                          add_frontier(Node, NbrNode, NodeDist + NbrDist, Acc)
                      end
                  end
              end, State1, Neighbors))
      end
  end.

is_closed(Node, State) ->
  maps:is_key(Node, State#state.closed).

is_in_frontier(Node, State) ->
  maps:is_key(Node, State#state.distances).

%% Pop the node from the frontier set with smallest distance from
%% origin.
take_frontier(State) ->
  {Elem, F0} = gb_sets:take_smallest(State#state.frontier),
  {Elem, State#state{frontier = F0}}.

%% Add a node to the frontier.
add_frontier(Parent, Node, Dist, State) ->
  F0 = gb_sets:add({Dist, Node}, State#state.frontier),
  D0 = maps:put(Node, Dist, State#state.distances),
  P0 = maps:put(Node, Parent, State#state.parents),
  State#state{ frontier = F0
             , distances = D0
             , parents = P0
             }.

%% Update the distance to a node in the frontier set.
update_distance(Parent, Node, NewDist, State) ->
  D0 = maps:put(Node, NewDist, State#state.distances),
  P0 = maps:put(Node, Parent, State#state.parents),
  State#state{ distances = D0
             , parents = P0
             }.

add_to_closed(Node, State) ->
  State#state{closed = maps:put(Node, true, State#state.closed)}.

%%% ============================================================
%%% Tests
%%% ============================================================

dijkstra_test_() ->
  Graph = #{
            a => [{7, e}, {3, b}, {5, d}],
            b => [{4, c}],
            c => [],
            d => [{6, c}],
            e => [{3, c}]
           },

  NbrFun = fun(Node, G) -> maps:get(Node, G) end,
  EndFun = fun(_Node, _Graph) -> false end,

  ?_assertEqual({7, [a, b, c]},
                begin
                  {finished, State} = dijkstra(Graph, a, NbrFun, EndFun),
                  {shortest_dist(State, c),
                   shortest_path(State, c)}
                end).

grid_helper(Grid, Size) ->
  PosToCoord = fun({Start, _}) ->
                   {Start rem Size, Start div Size}
               end,

  Obstacles = sets:from_list(
                [PosToCoord(Pos)
                 || Pos <- binary:matches(Grid, <<"#">>)]),

  GetPosOf = fun(Binary) ->
                 [Pos] = binary:matches(Grid, Binary),
                 PosToCoord(Pos)
             end,

  Goal = GetPosOf(<<"G">>),
  Start = GetPosOf(<<"S">>),
  MinX = MinY = 0,
  MaxX = MaxY = Size - 1,

  AdjFn = fun({X, Y}) ->
              [{Xa, Ya} ||
                Xa <- lists:seq(X - 1, X + 1),
                Ya <- lists:seq(Y - 1, Y + 1),
                Xa >= MinX, Xa =< MaxX,
                Ya >= MinY, Ya =< MaxY,
                {Xa, Ya} /= {X, Y}]
          end,

  NbrFn = fun(Curr, _) ->
              lists:filtermap(fun(N) ->
                                  case sets:is_element(N, Obstacles) of
                                    true -> false;
                                    false -> {true, {1, N}}
                                  end
                              end, AdjFn(Curr))
          end,

  EndFun = fun(Pos, _) -> Pos =:= Goal end,

  {found, State} = dijkstra(Grid, Start, NbrFn, EndFun),
  ?debugFmt("~nShortest path: ~p",
            [shortest_path(State, Goal)]),
  shortest_dist(State, Goal) + 1.

t1_test() ->
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
  ?assertEqual(24, grid_helper(Grid, 10)).
