-module(puzzle5).
-export([main/0]).
-compile([export_all]).

main() ->
    {ok, Bin} = file:read_file("input.txt"),
    Input = string:trim(binary_to_list(Bin)),
    {{part1, start1(Input)},
     {part2, start2(Input)}}.

start1(L) ->
    length(react(L)).

start2(L) ->
    lists:min([start1([C || C <- L, C =/= X, C =/= X - 32]) || 
                  X <- lists:seq($a, $z)]).

react(L) ->
    react(L, []).
             
react([], L) ->
    L;
react([C1|L1], [C2|L2]) when abs(C1 - C2) == 32 ->
    react(L1, L2);
react([C|L1], L2) ->
    react(L1, [C|L2]).

       
