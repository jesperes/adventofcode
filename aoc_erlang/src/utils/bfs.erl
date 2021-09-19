-module(bfs).

-include_lib("eunit/include/eunit.hrl").

-export([ bfs/3
        , shortest_path/2
        ]).

-record(state,
        { source
        , graph
        , discovered
        , queue
        , parents
        , callbackfun
        }).

shortest_path(State, Node) ->
  shortest_path(State, Node, [Node]).

shortest_path(#state{parents = Parents} = State, Node, Path) ->
  case maps:get(Node, Parents, undef) of
    undef -> Path;
    Parent -> shortest_path(State, Parent, [Parent|Path])
  end.

bfs(Graph, Source, CallbackFun) ->
  State =
    #state{ source = Source
          , graph = Graph
          , discovered = sets:from_list([Source])
          , queue = queue:from_list([Source])
          , parents = #{}
          , callbackfun = CallbackFun
          },

  bfs0(State).

bfs0(#state{ queue = Queue
           , callbackfun = CallbackFun
           , graph = Graph
           } = State) ->

  case queue:out(Queue) of
    {empty, _} -> {finished, State};
    {{value, Node}, Q0} ->
      State0 = State#state{queue = Q0},

      case CallbackFun(Node, Graph) of
        found -> {found, Node, State0};
        Neighbors ->

          State1 =
            lists:foldl(
              fun(Nbr, #state{ queue = Q1
                             , parents = P1
                             , discovered = D1
                             } = Acc) ->
                  case sets:is_element(Nbr, D1) of
                    true -> Acc;
                    false ->
                      Acc#state{ queue = queue:in(Nbr, Q1)
                               , discovered = sets:add_element(Nbr, D1)
                               , parents = maps:put(Nbr, Node, P1)
                               }
                  end
              end, State0, Neighbors),

          bfs0(State1)
      end
  end.

%%% ============================================================
%%% Tests
%%% ============================================================

bfs_test_() ->
  Graph = #{
            a => [e, b, d],
            b => [c, e],
            c => [d],
            d => [e],
            e => []
           },

  NbrFun = fun(Node, G) ->
               maps:get(Node, G)
           end,

  ?_assertEqual([a, b, c],
                begin
                  {finished, State} = bfs(Graph, a, NbrFun),
                  shortest_path(State, c)
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
              %% ?debugFmt("~nEvaluating ~p", [Curr]),
              lists:filtermap(
                fun(N) ->
                    case sets:is_element(N, Obstacles) of
                      true -> false;
                      false -> {true, N}
                    end
                end, AdjFn(Curr))
          end,

  {found, Goal, State} = bfs(Grid, Start, NbrFn),
  P = shortest_path(State, Goal),
  length(P).


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
