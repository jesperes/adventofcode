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
    
    part1(Instrs).

toi(N) -> list_to_integer(N).


part1(Instrs) ->     
    part1(Instrs, sets:new()).

part1([], Set) ->
    sets:size(Set);
part1([{toggle, From, To} = X|Rest], Set) ->
    io:format("~w~n", [X]),
    Set0 = 
        lists:foldl(fun(Pos, Acc) ->
                            case sets:is_element(Pos, Acc) of
                                true ->
                                    sets:del_element(Pos, Acc);
                                false ->
                                    sets:add_element(Pos, Acc)
                            end
                    end, Set, lights(From, To)),
    part1(Rest, Set0);
part1([{turn_on, From, To} = X|Rest], Set) ->
    io:format("~w~n", [X]),
    Set0 =
        lists:foldl(fun(Pos, Acc) ->
                            sets:add_element(Pos, Acc)
                    end, Set, lights(From, To)),
    part1(Rest, Set0);
part1([{turn_off, From, To} = X|Rest], Set) ->
    io:format("~w~n", [X]),
    Set0 = 
        lists:foldl(fun(Pos, Acc) ->
                            sets:del_element(Pos, Acc)
                    end, Set, lights(From, To)),
    part1(Rest, Set0).

lights({X0, Y0}, {X1, Y1}) ->
    [{X, Y} || X <- lists:seq(X0, X1),
               Y <- lists:seq(Y0, Y1)].
