-module(aoc2015_day07).

-include_lib("eunit/include/eunit.hrl").

main_test() ->
  Lines = inputs:get_as_lines(2015, 7),
  List = lists:map(fun(Line) ->
                       list_to_tuple(lists:map(fun c/1, string:tokens(Line, " ")))
                   end, Lines),

  A = propagate_signals(List, #{}),
  ?assertEqual(956, A),

  A1 = propagate_signals(List, #{b => A}),
  ?assertEqual(40149, A1).

c(S) ->
  try list_to_integer(S)
  catch error:badarg -> list_to_atom(S)
  end.

propagate_signals(List, Map) ->
  case maps:is_key(a, Map) of
    true -> maps:get(a, Map);
    false ->
      propagate_signals(List, run_pass(List, Map))
  end.


read_value(A, _Map) when is_integer(A) ->
  A;
read_value(A, Map) ->
  maps:get(A, Map, undefined).

run_pass([], Map) -> Map;
run_pass([{WireA, '->', WireB}|Rest], Map) ->
  run_pass(Rest, do_unary_op(WireA, WireB, fun(A) -> A end, Map));
run_pass([{WireA, 'AND', WireB, _, WireC}|Rest], Map) ->
  run_pass(Rest, do_bin_op(WireA, WireB, WireC, fun(A, B) -> A band B end, Map));
run_pass([{WireA, 'OR', WireB, _, WireC}|Rest], Map) ->
  run_pass(Rest, do_bin_op(WireA, WireB, WireC, fun(A, B) -> A bor B end, Map));
run_pass([{'NOT', WireA, _, WireB}|Rest], Map) ->
  run_pass(Rest, do_unary_op(WireA, WireB, fun(A) -> bnot A end, Map));
run_pass([{WireA, 'LSHIFT', WireB, _, WireC}|Rest], Map) ->
  run_pass(Rest, do_bin_op(WireA, WireB, WireC, fun(A, B) -> A bsl B end, Map));
run_pass([{WireA, 'RSHIFT', WireB, _, WireC}|Rest], Map) ->
  run_pass(Rest, do_bin_op(WireA, WireB, WireC, fun(A, B) -> A bsr B end, Map)).

write_wire(Wire, Value, Map) ->
  maps:update_with(Wire, fun(V) -> V end, Value, Map).

do_unary_op(WireA, WireB, Fun, Map) ->
  A = read_value(WireA, Map),
  if (A =:= undefined) ->
      Map;
     true ->
      write_wire(WireB, Fun(A), Map)
  end.

do_bin_op(WireA, WireB, WireC, Fun, Map) ->
  A = read_value(WireA, Map),
  B = read_value(WireB, Map),
  if (A =:= undefined) or (B =:= undefined) ->
      Map;
     true ->
      write_wire(WireC, Fun(A, B), Map)
  end.
