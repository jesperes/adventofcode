%%% @author Jesper Eskilson <>
%%% @copyright (C) 2018, Jesper Eskilson
%%% @doc
%%%
%%% @end
%%% Created : 23 Dec 2018 by Jesper Eskilson <>

-module(puzzle23).
-export([main/0]).
-compile([export_all]).

main() ->
    {{part1, part1()},
     {part2, not_implemented}}.

part1() ->
    NanoBots = input("input.txt"),
    Strongest = strongest(NanoBots),
    InRange = inrange_of(Strongest, NanoBots),
    length(InRange).

part2() ->
    NanoBots = input("testinput2.txt"),
    find_best_point(NanoBots).

strongest(NanoBots) ->
    {Strongest, _} = 
        lists:foldl(fun({_, _, _, R} = Bot, {_, Max}) when R > Max ->
                            {Bot, R};
                       (_, Max) ->
                            Max
                    end, {undef, 0}, NanoBots),
    Strongest.

input(Filename) ->
    {ok, Binary} = file:read_file(Filename),
    Lines = string:tokens(binary_to_list(Binary), "\n"),
    lists:map(fun(Line) ->
                      ["pos", X, Y, Z, "r", R] = string:tokens(Line, "=<,> "),
                      {list_to_integer(X),
                       list_to_integer(Y),
                       list_to_integer(Z),
                       list_to_integer(R)}
              end, Lines).

manhattan_dist({X1, Y1, Z1}, {X2, Y2, Z2}) ->
    abs(X1 - X2) +
        abs(Y1 - Y2) +
        abs(Z1 - Z2).
    
%% Is Bot2 inrange of Bot1?
inrange_bot(Bot1, Bot2) ->
    {X1, Y1, Z1, R1} = Bot1,
    {X2, Y2, Z2, _} = Bot2,
    manhattan_dist({X1, Y1, Z1}, 
                   {X2, Y2, Z2}) =< R1.

inrange_of(Bot, NanoBots) ->
    lists:filter(fun(X) ->
                         inrange_bot(Bot, X)
                 end, NanoBots).

num_nanobots_in_range(P, NanoBots) ->
    lists:filter(fun({X0, Y0, Z0, R}) ->
                         manhattan_dist(P, {X0, Y0, Z0}) =< R
                 end, NanoBots).

find_best_point(NanoBots) ->
    Points =
        [ {{X, Y, Z}, num_nanobots_in_range({X, Y, Z}, NanoBots)} || 
            X <- lists:seq(10, 50),
            Y <- lists:seq(10, 50),
            Z <- lists:seq(10, 50)],
    
    lists:foldl(fun({Pos, NumB}, {_P, Max, _}) when length(NumB) > Max ->
                        {Pos, length(NumB), NumB};
                   (_, Max) ->
                        Max
                end, {undef, 0, []}, Points).
