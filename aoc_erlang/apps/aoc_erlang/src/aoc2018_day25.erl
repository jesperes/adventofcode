-module(aoc2018_day25).
-include_lib("eunit/include/eunit.hrl").

-define(MAX_CONSTELLATION_SEP, 3).

main_test_() ->
  Input = inputs:get_as_binary(2018, 25),
  {"Part 1", ?_assertEqual(318, part1(Input))}.

part1(Input) ->
  InitConsts = get_points(Input),
  merge(InitConsts).

merge(Consts) ->
  NewConst = merge_consts(Consts),

  Len1 = maps:size(Consts),
  Len2 = maps:size(NewConst),

  if Len1 =/= Len2 -> merge(NewConst);
     true -> maps:size(NewConst)
  end.

%% Merge all mergable constellations
merge_consts(Consts) ->
  lists:foldl(fun({C1, C2}, Map) ->
                  C2p = maps:get(C2, Map, []),
                  case maps:is_key(C1, Map) of
                    true ->
                      maps:update_with(C1,
                                       fun(X) ->
                                           X ++ C2p
                                       end, maps:remove(C2, Map));
                    _ ->
                      Map
                  end
              end, Consts, mergable_consts(Consts)).

%% Return a list of tuples {C1, C2} of mergable constellations.
mergable_consts(Consts) ->
  Nums = maps:keys(Consts),
  [{C1, C2} ||
    C1 <- Nums,
    C2 <- Nums,
    C1 < C2,
    is_mergeable(C1, C2, Consts)].

%% Generalized manhattan distance
dist({X1, Y1, Z1, W1}, {X2, Y2, Z2, W2}) ->
  abs(X1 - X2) + abs(Y1 - Y2) + abs(Z1 - Z2) + abs(W1 - W2).

%% Two constellations are mergeable if there is at least one pair of
%% points separated by no more than MAX_CONSTELLATION_SEP.
is_mergeable(C1, C2, Consts) ->
  C1p = maps:get(C1, Consts, []),
  C2p = maps:get(C2, Consts, []),
  is_mergable0(C1p, C2p).

is_mergable0([], _) -> false;
is_mergable0([A|As], Bs) ->
  is_mergable1(A, Bs) orelse is_mergable0(As, Bs).

is_mergable1(_, []) -> false;
is_mergable1(A, [B|Bs]) ->
  (dist(A, B) =< ?MAX_CONSTELLATION_SEP) orelse is_mergable1(A, Bs).

%%% Parser
get_points(Binary) ->
  {_, Map} =
    lists:foldl(fun(Line, {N, Map}) ->
                    Point = list_to_tuple(
                              lists:map(fun list_to_integer/1,
                                        string:tokens(Line, ","))),
                    {N + 1, maps:put(N, [Point], Map)}
                end, {0, #{}}, string:tokens(binary_to_list(Binary), "\n\r")),
  Map.
