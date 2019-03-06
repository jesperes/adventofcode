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
                [pos_to_key(X, Y)
                 || X <- lists:seq(X0, X1),
                    Y <- lists:seq(Y0, Y1)]).

pos_to_key(X, Y) ->
    X + Y * 1000.

%% -- grid abstraction --

-define(GRID_TYPE_ARRAY, true).
%% -define(GRID_TYPE_MAP, true).

-ifdef(GRID_TYPE_ARRAY).

grid_new() -> array:new({default, false}),
               array:new({default, 0})}.

grid_get_solution(Grid) ->
    {Array1, Array2} = Grid,
    {length(array:sparse_to_list(Array1)),
     lists:sum(array:sparse_to_list(Array2))}.

grid_toggle(Pos, Grid) ->
    {Array1, Array2} = Grid,     
    
    {case array:get(Pos, Array1) of
         true -> array:reset(Pos, Array1);
         false -> array:set(Pos, true, Array1)
     end,
     array:set(Pos, array:get(Pos, Array2) + 2, Array2)}.

grid_turn_on(Pos, Grid) ->
    {Array1, Array2} = Grid,
    {array:set(Pos, true, Array1),
     array:set(Pos, array:get(Pos, Array2) + 1, Array2)}.

grid_turn_off(Pos, Grid) ->
    {Array1, Array2} = Grid,
    {array:reset(Pos, Array1),
     array:set(Pos, max(0, array:get(Pos, Array2) - 1), Array2)}.

-endif.

-ifdef(GRID_TYPE_MAP).

grid_new() -> #{}.

grid_get_solution(Grid) ->
    Values = maps:values(Grid),
    {length(lists:filter(fun({V, _}) -> V end, Values)),
     lists:sum(lists:map(fun({_, V}) -> V end, Values))}.

grid_toggle(Pos, Grid) ->
    maps:update_with(Pos, 
                     fun({S, B}) -> 
                             {not S, B + 2}
                     end, {true, 2}, Grid).

grid_turn_on(Pos, Grid) ->
    maps:update_with(Pos, 
                     fun({_S, B}) -> 
                             {true, B + 1}
                     end, {true, 1}, Grid).

grid_turn_off(Pos, Grid) ->
    maps:update_with(Pos, 
                     fun({_S, B}) -> 
                             {false, max(0, B - 1)}
                     end, {false, 0}, Grid).

-endif.
