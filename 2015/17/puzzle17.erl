%%% @author Jesper Eskilson <>
%%% @copyright (C) 2018, Jesper Eskilson
%%% @doc
%%%
%%% @end
%%% Created : 21 Dec 2018 by Jesper Eskilson <>

-module(puzzle17).
-export([start/0]).

combinations([]) ->
    [];
combinations([H | T]) ->
    CT = combinations(T),
    [[H]] ++ [[H | L] || L <- CT] ++ CT.

start() ->
    {ok, Binary} = file:read_file("input.txt"),
    Volume = 150,
    Buckets =
        lists:map(fun list_to_integer/1, 
                  string:tokens(binary_to_list(Binary), " \n\r")),

    BucketCombos =
      lists:filter(fun(BucketList) ->
                           lists:sum(BucketList) == Volume
                   end, combinations(Buckets)),
    
    SortedOnLength = 
        lists:sort(fun(A, B) ->
                           length(A) =< length(B)
                   end, BucketCombos),
    
    MinLen = length(lists:nth(1, SortedOnLength)),

    NumShortestCombos =
        length(lists:filter(fun(X) ->
                          length(X) == MinLen
                            end, SortedOnLength)),
    
    {length(BucketCombos),
     NumShortestCombos}.

    
    

