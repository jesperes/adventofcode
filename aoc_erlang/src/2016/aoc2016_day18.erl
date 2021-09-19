-module(aoc2016_day18).
-include_lib("eunit/include/eunit.hrl").

-define(TRAP, $^).
-define(SAFE, $.).

realdata() ->
  ".^..^....^....^^.^^.^.^^.^.....^.^..^...^^^^^^.^^^^.^.^^^^^^^.^^^^^..^.^^^.^^..^.^^.^....^.^...^^.^.".

next_row([Center, Right|Row]) ->
  next_row(?SAFE, Center, Right, Row).

next_row(Left, Center, Right, []) ->
  [is_trap(Left, Center, Right),
   is_trap(Center, Right, ?SAFE)];
next_row(Left, Center, Right, [NextRight|Rest]) ->
  Trap = is_trap(Left, Center, Right),
  [Trap|next_row(Center, Right, NextRight, Rest)].

is_trap(?TRAP, ?TRAP, ?SAFE) -> ?TRAP;
is_trap(?SAFE, ?TRAP, ?TRAP) -> ?TRAP;
is_trap(?TRAP, ?SAFE, ?SAFE) -> ?TRAP;
is_trap(?SAFE, ?SAFE, ?TRAP) -> ?TRAP;
is_trap(_, _, _) -> ?SAFE.

safe_tiles(Row) ->
  length(lists:filter(fun(C) -> C == ?SAFE end, Row)).

count_tiles(Start, Rows) ->
  {Tiles, _} =
    lists:foldl(fun(_, {N, AccIn}) ->
                    AccOut = next_row(AccIn),
                    {N + safe_tiles(AccOut), AccOut}
                end, {safe_tiles(Start), Start}, lists:seq(1, Rows - 1)),
  Tiles.

next_row_ex1_test() ->
  ?assertEqual(".^^^^", next_row("..^^.")),
  ?assertEqual("^^..^", next_row(".^^^^")).

next_row_ex2_test() ->
  ?assertEqual(38, count_tiles(".^^.^.^^^^", 10)).

main_test_() ->
  [ {"Part 1", ?_assertEqual(2035, count_tiles(realdata(), 40))}
  , {"Part 2", ?_assertEqual(20000577, count_tiles(realdata(), 400 * 1000))}
  ].
