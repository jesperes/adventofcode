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

grid_new() -> #{}.

grid_get_solution(Grid) -> 
    maps:fold(fun(_, {Vs, Vb}, {S, B}) ->
                      {if Vs -> S + 1; true -> S end, B + Vb}
              end, {0, 0}, Grid).

grid_toggle(Pos, Grid) ->
    maps:update_with(Pos, fun({S, B}) -> {not S, B + 2} end, {true, 2}, Grid).

grid_turn_on(Pos, Grid) ->
    maps:update_with(Pos, fun({_, B}) -> {true, B + 1} end, {true, 1}, Grid).

grid_turn_off(Pos, Grid) ->
    maps:update_with(Pos, fun({_, B}) -> {false, max(0, B - 1)} end, {false, 0}, Grid).


     
    
