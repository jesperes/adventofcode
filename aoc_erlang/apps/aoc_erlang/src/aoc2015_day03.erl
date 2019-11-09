-module(aoc2015_day03).

-include_lib("eunit/include/eunit.hrl").

-type pos() :: { integer(), integer() }.

main_test_() ->
  List = inputs:get_as_string(2015, 3),
  StartPos = {0, 0},

  [ {"Part 1",
     fun() ->
         ?assertEqual(2572, part1(List, StartPos))
     end}
  , {"Part 2",
     fun() ->
         ?assertEqual(2631, part2(List, StartPos))
     end}
  ].

part1(List, StartPos) ->
  {_, Presents} =
    lists:foldl(fun(C, {Pos, Map}) ->
                    NewPos = next_pos(C, Pos),
                    {NewPos, incr_map_cntr(NewPos, Map)}
                end, {StartPos, #{StartPos => 1}}, List),
  maps:size(Presents).

part2(List, StartPos) ->
  {{_, _}, Presents} =
    lists:foldl(fun(C, {{Pos, PosOther}, Map}) ->
                    NewPos = next_pos(C, Pos),
                    {{PosOther, NewPos}, incr_map_cntr(NewPos, Map)}
                end, {{StartPos, StartPos}, #{StartPos => 2}}, List),
  maps:size(Presents).

-spec incr_map_cntr(pos(), map()) -> map().
incr_map_cntr(Key, Map) ->
  maps:update_with(Key, fun(V) -> V + 1 end, 1, Map).

-spec next_pos(integer(), pos()) -> pos().
next_pos($<, {X, Y}) -> {X-1, Y};
next_pos($>, {X, Y}) -> {X+1, Y};
next_pos($v, {X, Y}) -> {X, Y+1};
next_pos($^, {X, Y}) -> {X, Y-1}.
