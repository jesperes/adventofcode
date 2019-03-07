-module(puzzle25).
-export([start/0]).
-include_lib("eunit/include/eunit.hrl").

init_value() -> 20151125.
next_value(Prev) -> (Prev * 252533) rem 33554393.

start() ->
    find_value(3019, 3010).

find_value(1, 1) -> init_value();
find_value(1, Y) -> next_value(find_value(Y - 1, 1));
find_value(X, Y) -> next_value(find_value(X - 1, Y + 1)).

%%% --- Tests ---
next_value_test() ->
    X = init_value(),
    ?assertEqual(31916031, next_value(X)).

find_value_test() ->
    ?assertEqual(21629792, find_value(2, 2)),
    ?assertEqual(27995004, find_value(6, 6)),
    ?assertEqual(8997277, find_value(3019, 3010)).
