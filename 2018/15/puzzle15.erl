%%% @author Jesper Eskilson <jesper.eskilson@klarna.com>
%%% @copyright (C) 2019, Jesper Eskilson
%%% @doc
%%%
%%% @end
%%% Created :  2 Feb 2019 by Jesper Eskilson <jesper.eskilson@klarna.com>

-module(puzzle15).
-export([main/0]).

main() ->
    {{part1, part1()},
     {part2, 0}}.

parse_grid(Binary) ->
    xy_fold(
      fun(X, Y, C, Acc) when C =/= $. ->
              [{X, Y, list_to_atom([C])}|Acc];
         (_, _, _, Acc) ->
              Acc
      end, [], Binary).

part1() ->    
    {ok, Binary} = file:read_file("testinput2.txt"),
    parse_grid(Binary).
    
%%% Utilities    

split(Binary) ->
    Str = binary_to_list(Binary),
    Lines = [First|_] = string:tokens(Str, "\r\n"),
    {length(First), length(Lines), Str}.

xy_fold(Fun, Init, Binary) ->
    {Width, Height, Str} = split(Binary),
    {_, Out} = 
        lists:foldl(fun(C, Acc) when (C =:= $\n) or (C =:= $\r) ->
                            Acc;
                       (C, {N, AccIn}) ->
                            X = N rem (Width + 1),
                            Y = N div (Width + 1),
                            {N + 1, Fun(X, Y, C, AccIn)}
                    end, {0, Init}, Str),
    {Width, Height, Out}.
