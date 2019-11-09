-module(aoc2015_day25).

-include_lib("eunit/include/eunit.hrl").

init_value() -> 20151125.
next_value(Prev) -> (Prev * 252533) rem 33554393.

main_test_() ->
  {"Finale",
   fun() ->
       ?assertEqual(8997277, find_value(3019, 3010))
   end}.

find_value(1, 1) -> init_value();
find_value(1, Y) -> next_value(find_value(Y - 1, 1));
find_value(X, Y) -> next_value(find_value(X - 1, Y + 1)).

%%% --- Tests ---
unit_test_() ->
  {"Unit tests",
   [ {"Next value", fun next_value_t/0}
   , {"Find value", fun find_value_t/0}
   ]}.

next_value_t() ->
  X = init_value(),
  ?assertEqual(31916031, next_value(X)).

find_value_t() ->
  ?assertEqual(21629792, find_value(2, 2)),
  ?assertEqual(27995004, find_value(6, 6)),
  ?assertEqual(8997277, find_value(3019, 3010)).
