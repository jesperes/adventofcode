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
    
    {part1(Instrs), part2(Instrs)}.

toi(N) -> list_to_integer(N).


part1(Instrs) ->     
    part1(Instrs, grid_new()).

part1([], Grid) ->
    grid_size(Grid);
part1([{toggle, From, To}|Rest], Grid) ->
    part1(Rest, fold_xy(fun grid_toggle/2, Grid, From, To));
part1([{turn_on, From, To}|Rest], Grid) ->
    part1(Rest, fold_xy(fun grid_turn_on/2, Grid, From, To));
part1([{turn_off, From, To}|Rest], Grid) ->
    part1(Rest, fold_xy(fun grid_turn_off/2, Grid, From, To)).

part2(Instrs) ->     
    part2(Instrs, grid_new()).

part2([], Grid) ->
    grid_sum_keys(Grid);
part2([{toggle, From, To}|Rest], Grid) ->
    part2(Rest, fold_xy(fun grid_incr_2/2, Grid, From, To));
part2([{turn_on, From, To}|Rest], Grid) ->
    part2(Rest, fold_xy(fun grid_incr_1/2, Grid, From, To));
part2([{turn_off, From, To}|Rest], Grid) ->
    part2(Rest, fold_xy(fun grid_decr_1/2, Grid, From, To)).

fold_xy(Fun, Init, {X0, Y0}, {X1, Y1}) ->
    lists:foldl(Fun, Init, 
                [{X, Y} || X <- lists:seq(X0, X1),
                           Y <- lists:seq(Y0, Y1)]).

grid_new() -> #{}.

grid_size(Grid) -> maps:size(maps:filter(fun(_, V) -> V end, Grid)).

grid_toggle(Pos, Grid) ->
    maps:update_with(Pos, fun(V) -> not V end, true, Grid).

grid_turn_on(Pos, Grid) ->
    maps:put(Pos, true, Grid).

grid_turn_off(Pos, Grid) ->
    maps:remove(Pos, Grid).

grid_incr_2(Pos, Grid) ->
    maps:update_with(Pos, fun(V) -> V + 2 end, 2, Grid).

grid_incr_1(Pos, Grid) ->
    maps:update_with(Pos, fun(V) -> V + 1 end, 1, Grid).

grid_decr_1(Pos, Grid) ->
    maps:update_with(Pos, fun(V) -> max(V - 1, 0) end, 0, Grid).

grid_sum_keys(Grid) ->                                   
    lists:sum(maps:values(Grid)).
     
    
