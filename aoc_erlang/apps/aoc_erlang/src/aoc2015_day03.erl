-module(aoc2015_day03).

-include_lib("eunit/include/eunit.hrl").

-type pos() :: { integer(), integer() }.

-spec part1_test() -> integer().
part1_test() ->
  List = inputs:get_as_string(2015, 3),
  StartPos = {0, 0},
  {_, Presents} =
    lists:foldl(fun(C, {Pos, Map}) ->
                    NewPos = next_pos(C, Pos),
                    {NewPos, incr_map_cntr(NewPos, Map)}
                end, {StartPos, #{StartPos => 1}}, List),
  2572 = maps:size(Presents).

-spec part2_test() -> integer().
part2_test() ->
  List = inputs:get_as_string(2015, 3),
  StartPos = {0, 0},
  {{_, _}, Presents} =
    lists:foldl(fun(C, {{Pos, PosOther}, Map}) ->
                    NewPos = next_pos(C, Pos),
                    {{PosOther, NewPos}, incr_map_cntr(NewPos, Map)}
                end, {{StartPos, StartPos}, #{StartPos => 2}}, List),
  2631 = maps:size(Presents).

-spec incr_map_cntr(pos(), map()) -> map().
incr_map_cntr(Key, Map) ->
  maps:update_with(Key, fun(V) -> V + 1 end, 1, Map).

-spec next_pos(integer(), pos()) -> pos().
next_pos($<, {X, Y}) -> {X-1, Y};
next_pos($>, {X, Y}) -> {X+1, Y};
next_pos($v, {X, Y}) -> {X, Y+1};
next_pos($^, {X, Y}) -> {X, Y-1}.
