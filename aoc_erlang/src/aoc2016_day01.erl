-module(aoc2016_day01).

-include_lib("eunit/include/eunit.hrl").

-define(INPUT, <<"L1, R3, R1, L5, L2, L5, R4, L2, R2, R2, L2, R1, L5, R3, L4, L1, L2, R3, R5, L2, R5, L1, R2, L5, R4, R2, R2, L1, L1, R1, L3, L1, R1, L3, R5, R3, R3, L4, R4, L2, L4, R1, R1, L193, R2, L1, R54, R1, L1, R71, L4, R3, R191, R3, R2, L4, R3, R2, L2, L4, L5, R4, R1, L2, L2, L3, L2, L1, R4, R1, R5, R3, L5, R3, R4, L2, R3, L1, L3, L3, L5, L1, L3, L3, L1, R3, L3, L2, R1, L3, L1, R5, R4, R3, R2, R3, L1, L2, R4, L3, R1, L1, L1, R5, R2, R4, R5, L1, L1, R1, L2, L4, R3, L1, L3, R5, R4, R3, R3, L2, R2, L1, R4, R2, L3, L4, L2, R2, R2, L4, R3, R5, L2, R2, R4, R5, L2, L3, L2, R5, L4, L2, R3, L5, R2, L1, R1, R3, R3, L5, L2, L2, R5">>).

main_test_() ->
  {"Part 1 & 2", fun start/0}.

start() ->
  {FinalPos, _, FinalVT, _} =
    lists:foldl(
      fun([Turn|Rest], {Pos, Dir, VT, Visited}) ->
          NewDir = turn(Turn, Dir),
          {NewPos, VT0, V0} =
            visit(Pos, delta(NewDir), list_to_integer(Rest),
                  VT, Visited),
          {NewPos, NewDir, VT0, V0}
      end,
      {{0, 0}, 0, undef, sets:new()},
      string:tokens(binary_to_list(?INPUT), ", ")),

  ?assertEqual(278, dist(FinalPos)),
  ?assertEqual(161, dist(FinalVT)).

dist({X, Y}) -> abs(X) + abs(Y).

turn($R, Dir) -> (Dir + 1) rem 4;
turn($L, Dir) -> (Dir + 3) rem 4.

delta(0) -> {0, -1};
delta(1) -> {1, 0};
delta(2) -> {0, 1};
delta(3) -> {-1, 0}.

visit(Pos, _, 0, VT, Visited) ->
  {Pos, VT, Visited};
visit({X, Y}, {Dx, Dy}, Steps, undef, Visited) ->
  VT = case sets:is_element({X, Y}, Visited) of
         true -> {X, Y};
         false -> undef
       end,
  visit({X + Dx, Y + Dy}, {Dx, Dy}, Steps - 1, VT,
        sets:add_element({X, Y}, Visited));
visit({X, Y}, {Dx, Dy}, Steps, VT, Visited) ->
  visit({X + Dx, Y + Dy}, {Dx, Dy}, Steps - 1, VT,
        sets:add_element({X, Y}, Visited)).
