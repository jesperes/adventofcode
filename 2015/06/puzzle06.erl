-module(puzzle06).
-export([start/0]).

start() ->
    {ok, Bin} = file:read_file("input.txt"),
    Lines = string:tokens(binary_to_list(Bin), "\n"),
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

    process_instr(Instrs).

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
                [<<X:16, Y:16>> || X <- lists:seq(X0, X1),
                                   Y <- lists:seq(Y0, Y1)]).
%% -- grid abstraction --

array_pos_to_index(<<X:16, Y:16>>) ->
    X + Y * 1000.

grid_new() -> {array:new({default, false}),
               array:new({default, 0})}.

grid_get_solution(Grid) ->
    {Array1, Array2} = Grid,
    {length(array:sparse_to_list(Array1)),
     lists:sum(array:sparse_to_list(Array2))}.

grid_toggle(Pos, Grid) ->
    {Array1, Array2} = Grid,
    Idx = array_pos_to_index(Pos),
    {array:set(Idx, not array:get(Idx, Array1), Array1),
     array:set(Idx, array:get(Idx, Array2) + 2, Array2)}.

grid_turn_on(Pos, Grid) ->
    {Array1, Array2} = Grid,
    Idx = array_pos_to_index(Pos),
    {array:set(Idx, true, Array1),
     array:set(Idx, array:get(Idx, Array2) + 1, Array2)}.

grid_turn_off(Pos, Grid) ->
    {Array1, Array2} = Grid,
    Idx = array_pos_to_index(Pos),
    {array:set(Idx, false, Array1),
     array:set(Idx, max(0, array:get(Idx, Array2) - 1), Array2)}.
