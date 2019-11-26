-module(aoc2016_day24).
-define(WALL, $#).

-include_lib("eunit/include/eunit.hrl").

main_test_() ->
  Bin = inputs:get_as_binary(2016, 24),

  [ {"Part 1", ?_assertEqual(462, start([], Bin))}
  , {"Part 2", ?_assertEqual(676, start([0], Bin))}
  ].

example_test_() ->
  Bin =
    <<"###########\n",
      "#0.1.....2#\n",
      "#.#######.#\n",
      "#4.......3#\n",
      "###########\n">>,
  {"Example", ?_assertEqual(14, start([], Bin))}.

start(LastNode, Binary) ->
  Map = parse_binary(Binary),
  Numbers = maps:filter(fun(K, _V) ->
                            if (K >= 0) and (K =< 9) -> true;
                               true -> false
                            end
                        end, Map),

  Nums = maps:keys(Numbers),

  DistMap = compute_distances(Numbers, Map),
  Combinations =
    lists:map(fun(Perm) ->
                  [0] ++ Perm ++ LastNode
              end, permute(Nums -- [0])),

  lists:min(
    lists:map(fun(Combo) ->
                  combo_distance(Combo, DistMap)
              end, Combinations)).

%% Compute the distance for a given order of numbers.
combo_distance(X, _) when length(X) =< 1 -> 0;
combo_distance([A, B|Rest], DistMap) when A =< B ->
  maps:get({A, B}, DistMap) + combo_distance([B|Rest], DistMap);
combo_distance([A, B|Rest], DistMap) ->
  maps:get({B, A}, DistMap) + combo_distance([B|Rest], DistMap).

%% Compute a map of {X, Y} -> Dist for all combinations numbers
%% occurring in the map.
compute_distances(NumberMap, Map) ->
  Numbers = maps:keys(NumberMap),
  Distances =
    [ begin
        {Dist, _} =
          find_shortest_path(maps:get(X, NumberMap),
                             maps:get(Y, NumberMap), Map),
        {X, Y, Dist}
      end || X <- Numbers,
             Y <- Numbers,
             X =< Y],

  lists:foldl(fun({X, Y, Dist}, Acc) ->
                  maps:put({X, Y}, Dist, Acc)
              end, #{}, Distances).

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
