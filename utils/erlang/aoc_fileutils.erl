%%% @author Jesper Eskilson <jesper.eskilson@klarna.com>
%%% @copyright (C) 2019, Jesper Eskilson
%%% @doc
%%%
%%% @end
%%% Created :  2 Feb 2019 by Jesper Eskilson <jesper.eskilson@klarna.com>

-module(aoc_fileutils).
-export([xy_fold/3]).

%% Folds a fun over a binary representing a x,y-grid, with line
%% separated by '\n'.
xy_fold(Fun, Init, Binary) ->
    Str = binary_to_list(Binary),
    [First|_] = string:split(Str, "\r\n"),
    Width = length(First),
    {_, Out} = 
        lists:foldl(fun(C, Acc) when (C =:= $\n) or (C =:= $\r) ->
                            Acc;
                       (C, {N, AccIn}) ->
                            X = N rem (Width + 1),
                            Y = N div (Width + 1),
                            {N + 1, Fun(X, Y, C, AccIn)}
                    end, {0, Init}, Str),
    Out.
