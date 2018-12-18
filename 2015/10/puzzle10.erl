%%% @author Jesper Eskilson <>
%%% @copyright (C) 2018, Jesper Eskilson
%%% @doc
%%%
%%% @end
%%% Created : 18 Dec 2018 by Jesper Eskilson <>

-module(puzzle10).

-compile([export_all]).

input() -> 
    "1321131112".

start() ->
    test(),
    length(
      lists:foldl(fun(_, S) ->
                          look_and_say(S)
                  end, input(), lists:seq(1, 50))).

test() ->
    "11" = look_and_say("1"),
    "21" = look_and_say("11"),
    "1211" = look_and_say("21").

look_and_say([]) ->
    [];
look_and_say([X|_] = Str) ->
    Prefix = lists:takewhile(fun(C) -> X == C end, Str),
    Len = length(Prefix),
    {_, L2} = lists:split(Len, Str),
    integer_to_list(Len) ++
        [X] ++ look_and_say(L2).
    

    
                             


