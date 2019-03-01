%%% @author Jesper Eskilson <>
%%% @copyright (C) 2018, Jesper Eskilson
%%% @doc
%%%
%%% @end
%%% Created : 17 Dec 2018 by Jesper Eskilson <>

-module(puzzle08).
-export([start/0]).

start() ->
    {ok, Binary} = file:read_file("input.txt"),
    Part1Sol = 
        lists:foldl(fun({LSize, MSize}, Acc) ->
                            (LSize - MSize) + Acc
                    end, 0,
                    [measure(Str) || 
                        Str <- string:tokens(binary_to_list(Binary), "\n")]),
    
    Part2Sol =
        lists:foldl(fun({Orig, Quoted}, Acc) ->
                            Acc + (length(Quoted) - length(Orig))
                    end, 0,
                    [{Str, "\"" ++ quote(Str) ++ "\""} || 
                        Str <- string:tokens(binary_to_list(Binary), "\n")]),
   
    {Part1Sol, Part2Sol}.
 
measure(Str) ->
    measure(Str, before).
measure([], 'after') ->
    {0, 0};
measure([$"|Rest], before) ->
    {LSize, MSize} = measure(Rest, inside),
    {LSize + 1, MSize};
measure([$"|Rest], inside) ->
    {LSize, MSize} = measure(Rest, 'after'),
    {LSize + 1, MSize};
measure([$\\,$x,_,_|Rest], inside) ->
    %% Hexadecimal literal: \xDD
    {LSize, MSize} = measure(Rest, inside),
    {LSize + 4, MSize + 1};
measure([$\\,$"|Rest], inside) ->
    %% Quoted literal: \"
    {LSize, MSize} = measure(Rest, inside),
    {LSize + 2, MSize + 1};
measure([$\\,$\\|Rest], inside) ->
    %% Quoted backslash: \\
    {LSize, MSize} = measure(Rest, inside),
    {LSize + 2, MSize + 1};
measure([_|Rest], inside) ->
    %% Literal char
    {LSize, MSize} = measure(Rest, inside),
    {LSize + 1, MSize + 1}.
  
    

quote([]) ->
    "";
quote([$"|Rest]) ->
    [$\\, $"|quote(Rest)];
quote([$\\|Rest]) ->
    [$\\, $\\|quote(Rest)];
quote([C|Rest]) ->
    [C|quote(Rest)].
   
  

