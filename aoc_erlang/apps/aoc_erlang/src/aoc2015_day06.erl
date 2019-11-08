-module(aoc2015_day06).

-include_lib("eunit/include/eunit.hrl").

%% This solution runs in ~15 seconds, but eunit times out at 5 secs.
main_with_timeout_test_() ->
  {timeout, 60, fun main/0}.

main() ->
  Lines = inputs:get_as_lines(2015, 6),
  Instrs =
    lists:map(
      fun(Line) ->
          case string:tokens(Line, " ,") of
            ["toggle", X0, Y0, "through", X1, Y1] ->
              {toggle, {toi(X0), toi(Y0)}, {toi(X1), toi(Y1)}};
            ["turn", "on", X0, Y0, "through", X1, Y1] ->
              {turn_on, {toi(X0), toi(Y0)}, {toi(X1), toi(Y1)}};
            ["turn", "off", X0, Y0, "through", X1, Y1] ->
              {turn_off, {toi(X0), toi(Y0)}, {toi(X1), toi(Y1)}}
          end
      end, Lines),

  {543903, 14687245} = process_instr(Instrs).

toi(N) -> list_to_integer(N).

process_instr(Instrs) ->
  process_instr(Instrs, grid_new()).

process_instr([], Grid) ->
  grid_get_solution(Grid);
process_instr([{toggle, From, To}|Rest], Grid) ->
  process_instr(Rest, fold_xy(fun grid_toggle/2, Grid, From, To));
process_instr([{turn_on, From, To}|Rest], Grid) ->
  process_instr(Rest, fold_xy(fun grid_turn_on/2, Grid, From, To));
process_instr([{turn_off, From, To}|Rest], Grid) ->
  process_instr(Rest, fold_xy(fun grid_turn_off/2, Grid, From, To)).

fold_xy(Fun, Init, {X0, Y0}, {X1, Y1}) ->
  lists:foldl(Fun, Init,
              [X + Y * 1000 ||
                X <- lists:seq(X0, X1),
                Y <- lists:seq(Y0, Y1)]).

%% -- grid abstraction --

grid_new() -> array:new({default, {false, 0}}).

grid_get_solution(Grid) ->
  Values = array:sparse_to_list(Grid),
  {length(lists:filter(fun({V, _}) -> V end, Values)),
   lists:sum(lists:map(fun({_, V}) -> V end, Values))}.

grid_toggle(Pos, Grid) ->
  {S, B} = array:get(Pos, Grid),
  array:set(Pos, {not S, B + 2}, Grid).

grid_turn_on(Pos, Grid) ->
  {_, B} = array:get(Pos, Grid),
  array:set(Pos, {true, B + 1}, Grid).

grid_turn_off(Pos, Grid) ->
  {_, B} = array:get(Pos, Grid),
  array:set(Pos, {false, max(0, B - 1)}, Grid).
