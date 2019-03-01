%%% @author Jesper Eskilson <>
%%% @copyright (C) 2018, Jesper Eskilson
%%% @doc
%%%
%%% @end
%%% Created : 18 Dec 2018 by Jesper Eskilson <>

-module(puzzle10).

% -export([start/0]).
-compile([export_all]).

input() -> 
    <<"1321131112">>.

start() ->
    test(),
    {part1(), part2()}.

part1() -> iterate(40).
part2() -> iterate(50).

iterate(N) ->
    Fun = fun(_, S) -> look_and_say(S) end,
    byte_size(lists:foldl(Fun, input(), lists:seq(1, N))).

test() ->
    <<"11">> = look_and_say(<<"1">>),
    <<"21">> = look_and_say(<<"11">>),
    <<"1211">> = look_and_say(<<"21">>).
            
look_and_say(Bin) ->
    list_to_binary(look_and_say0(Bin)).

look_and_say0(<<>>) -> <<>>;
look_and_say0(<<X,Rest/binary>> = Bin) ->
    Len = prefix_len(X, Bin),
    {_, L2} = split_binary(Bin, Len),
    P1 = integer_to_list(Len),
    [P1, [X], look_and_say0(L2)].

prefix_len(X, Bin) -> prefix_len(0, X, Bin, 0).
prefix_len(I, X, Bin, Acc) when I >= byte_size(Bin) -> Acc;
prefix_len(I, X, Bin, Acc) ->
    case binary:at(Bin, I) of
        X -> prefix_len(I + 1, X, Bin, Acc + 1);
        N -> Acc
    end.

            



    
                             


