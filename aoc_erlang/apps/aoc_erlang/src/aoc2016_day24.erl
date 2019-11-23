-module(aoc2016_day24).
-define(WALL, $#).

-include_lib("eunit/include/eunit.hrl").

main_test_() ->
  {"Part ?", ?_assertEqual(0, start())}.

start() ->
  Binary = inputs:get_as_binary(2016, 24),
  Map = parse_binary(Binary),
  Numbers = maps:filter(fun(K, _V) ->
                            if (K >= 0) and (K =< 9) -> true;
                               true -> false
                            end
                        end, Map),

  Nums = maps:keys(Numbers),
  %% Max = lists:max(Nums),
  Combinations =
    lists:map(fun(Perm) ->
                  [0] ++ Perm ++ [0]
              end, permute(Nums -- [0])),

  %% This is very inefficient, as it computes the shortest paths
  %% between two nodes over and over again, instead of storing the
  %% path once computed.

  NumCombos=length(Combinations),
  io:format("Number of combinations: ~p~n", [NumCombos]),

  {_, Steps, BestPath} =
    lists:foldl(fun(Order, {N, CurrMin, BestPath}) ->
                    erlang:display({processing,
                                    {current, N},
                                    {total, NumCombos},
                                    {percent,
                                     floor(100 * (N/NumCombos))}}),
                    Steps = visit_all_nodes(Order, Map),
                    FlattenedPath = lists:flatten(Steps),
                    NumSteps = length(FlattenedPath),
                    if NumSteps < CurrMin -> {N + 1, NumSteps, FlattenedPath};
                       true -> {N + 1, CurrMin, BestPath}
                    end
                end, {0, inf, undef}, Combinations),

  print_grid(Map, BestPath),
  {Steps, BestPath}.

%% Find the shortest path from Start to End.
find_shortest_path(Start, {Xe, Ye} = End, Grid) ->
  NbrFn =
    fun({X, Y}) ->
        lists:filter(
          fun(Pos) ->
              maps:get(Pos, Grid, undef) /= ?WALL
          end,
          [{X - 1, Y},
           {X + 1, Y},
           {X, Y + 1},
           {X, Y - 1}])
    end,
  DistFn = fun(_, _) -> 1 end,
  CostFn = fun({X, Y}) ->
               abs(Xe - X) + abs(Ye - Y)
           end,
  astar2:astar(Start, End, CostFn, NbrFn, DistFn).

%% Visit all nodes in the given order. Return a deep list where each
%% list contains the steps to move from one position to the next.
visit_all_nodes([_], _Grid) ->
  [];
visit_all_nodes([Start, End|Rest], Grid) ->
  StartPos = maps:get(Start, Grid),
  EndPos = maps:get(End, Grid),
  %% The A*-implementation will return the path in reverse order and
  %% including the start position, so to find the actual number of
  %% *steps*, we need to reverse the list and remove the start node.
  [StartPos|Steps] = lists:reverse(find_shortest_path(StartPos, EndPos, Grid)),
  [Steps|visit_all_nodes([End|Rest], Grid)].

%%% Pretty-printer

bounds(Grid) ->
  MaxY = maps:fold(fun({_, Y}, _, MaxY) when Y > MaxY -> Y;
                      (_, _, MaxY) -> MaxY
                   end, 0, Grid),
  MaxX = maps:fold(fun({X, _}, _, MaxX) when X > MaxX -> X;
                      (_, _, MaxX) -> MaxX
                   end, 0, Grid),
  {MaxX, MaxY}.

print_grid(Grid, Path) ->
  {MaxX, MaxY} = bounds(Grid),
  Str =
    [[pos_to_char({X, Y}, Grid, Path)
      || X <- lists:seq(0, MaxX)] ++ "\n"
     || Y <- lists:seq(0, MaxY)],
  io:format("~s~n", [Str]).

pos_to_char(Pos, Grid, Path) ->
  IsPath = lists:member(Pos, Path),
  IsWall = maps:get(Pos, Grid, undef) == ?WALL,
  if IsPath ->
      $o;
     IsWall ->
      ?WALL;
     true ->
      $.
  end.

%%% Parser

parse_binary(Binary) ->
  Lines = string:tokens(binary_to_list(Binary), "\n\r"),
  {_, Map} = lists:foldl(fun parse_line/2, {0, #{}}, Lines),
  Map.

parse_line(Line, {Y, Map}) ->
  {_, _, M0} = lists:foldl(fun parse_char/2, {0, Y, Map}, Line),
  {Y + 1, M0}.

parse_char(C, {X, Y, Map}) ->
  {X + 1, Y,
   if C == ?WALL -> Map#{{X, Y} => ?WALL};
      (C >= $0) and (C =< $9) -> Map#{C - $0 => {X, Y}};
      true -> Map
   end}.

%%% Helpers

permute([]) -> [[]];
permute(L) -> [[X|Y] || X <- L, Y <- permute(L -- [X])].
