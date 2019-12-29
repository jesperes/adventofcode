-module(dijkstra).

-include_lib("eunit/include/eunit.hrl").

-export([ dijkstra/3
        , shortest_dist/2
        , shortest_path/2
        , closed_set/1
        ]).

-record(state,
        { source
        , graph
        , closed
        , frontier
        , distances
        , parents
        , callbackfun
        }).

shortest_dist(#state{distances = Map}, Node) ->
  maps:get(Node, Map).

shortest_path(State, Node) ->
  shortest_path(State, Node, [Node]).

shortest_path(#state{parents = Parents} = State, Node, Path) ->
  case maps:get(Node, Parents, undef) of
    undef -> Path;
    Parent -> shortest_path(State, Parent, [Parent|Path])
  end.

closed_set(#state{closed = Closed}) ->
  lists:sort(maps:keys(Closed)).

%%
%% Apply dijkstra's algorithm to Graph, beginning at Source.
%%
%% @param Graph   The graph to search.
%% @param Source  Start node
%% @param CallbackFun
%%                Function for evaluating nodes. Should return a list
%%                of {Dist, Node} tuples for the nodes neighbors, or
%%                'found' if the node passed to it was the end node.
%% @returns       {finished, State} if there are no more nodes to
%%                evaluate, or {found, Node, State} if the goal node was
%%                found.
%%
-spec dijkstra(Graph :: term(),
               Source :: term(),
               CallbackFun :: fun((Node :: term(), Graph :: term()) ->
                                     list({NbrDist :: integer(),
                                           NbrNode :: term()}))) ->
                  {finished, Result :: #state{}} |
                  {found, Result :: #state{}}.
dijkstra(Graph, Source, CallbackFun) ->
  State =
    #state{ source = Source
          , graph = Graph
          , closed = #{}
          , frontier = gb_sets:from_list([{0, Source}])
          , distances = #{Source => 0}
          , parents = #{}
          , callbackfun = CallbackFun
          },

  dijkstra0(State).

dijkstra0(#state{ frontier = Frontier
                , callbackfun = CallbackFun
                , graph = Graph
                } = State) ->
  case gb_sets:is_empty(Frontier) orelse take_frontier(State) of
    true -> {finished, State};
    {{NodeDist, Node}, State0} ->

      %% Function to apply to each neighbor
      NbrFun =
        fun({NbrDist, NbrNode}, #state{distances = Dists} = Acc) ->
            case {is_closed(NbrNode, Acc),
                  is_in_frontier(NbrNode, Acc)} of
              {true, _} ->
                %% Node already evaluated
                Acc;
              {false, false} ->
                %% Unseen node
                add_frontier(Node, NbrNode,
                             NodeDist + NbrDist, Acc);
              {false, true} ->
                %% Previously seen, but not evaluated. Update
                %% shortest distance, if necessary.
                NewNbrDist = NodeDist + NbrDist,
                OldNbrDist = maps:get(NbrNode, Dists),
                if NewNbrDist < OldNbrDist ->
                    update_distance(Node, NbrNode, NewNbrDist, Acc);
                   true -> Acc
                end
            end
        end,

      case CallbackFun(Node, Graph) of
        found -> {found, Node, State0};
        Neighbors ->
          dijkstra0(
            lists:foldl(NbrFun,
                        add_to_closed(Node, State0),
                        Neighbors))
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

  ?_assertEqual({7, [a, b, c]},
                begin
                  {finished, State} = dijkstra(Graph, a, NbrFun),
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

  NbrFn = fun(Curr, _) when Curr =:= Goal ->
              found;
             (Curr, _) ->
              lists:filtermap(fun(N) ->
                                  case sets:is_element(N, Obstacles) of
                                    true -> false;
                                    false -> {true, {1, N}}
                                  end
                              end, AdjFn(Curr))
          end,

  {found, Goal, State} = dijkstra(Grid, Start, NbrFn),
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
