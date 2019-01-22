%%% @author Jesper Eskilson <jesper.eskilson@klarna.com>
%%% @copyright (C) 2019, Jesper Eskilson
%%% @doc
%%%
%%% @end
%%% Created : 17 Jan 2019 by Jesper Eskilson <jesper.eskilson@klarna.com>

-module(puzzle14).
-compile([export_all]).
-include_lib("eunit/include/eunit.hrl").

%% Chocolate Carts

main() ->
    Input = 633601,
    
    {{part1, start1(Input)},
     {part2, start2(Input)}}.
    
start1(Input) ->
    A = array:from_list([3, 7]),
    lists:map(fun(X) -> X + $0 end, step1(A, 0, 1, Input)).

start2(Input) ->
    Input0 = combine_recipes(Input, 0),
    A = array:from_list([3, 7]),
    step2(A, 0, 1, Input0, Input0).

step1(Array, Elf1, Elf2, Input) ->
    S = array:size(Array),
    if S > Input + 10 ->
            sub_array(Array, Input, 10);
       true ->
            X1 = array:get(Elf1, Array),
            X2 = array:get(Elf2, Array),
            NewArray = 
                lists:foldl(fun(N, Acc) ->
                                    array:set(array:size(Acc), N, Acc)
                            end, Array, combine_recipes(X1, X2)),
            Size = array:size(NewArray),
            NewElf1 = (Elf1 + X1 + 1) rem Size,
            NewElf2 = (Elf2 + X2 + 1) rem Size,
            step1(NewArray, NewElf1, NewElf2, Input)
    end.

step2(Array, _Elf1, _Elf2, Input, []) ->
    array:size(Array) - length(Input);
step2(Array, Elf1, Elf2, Input, Progress) ->
    X1 = array:get(Elf1, Array),
    X2 = array:get(Elf2, Array),
    {NewProg0, NewArray} = 
        lists:foldl(fun(N, {[H|NewProg], Acc}) ->
                            AccOut = array:set(array:size(Acc), N, Acc),
                            if N == H -> {NewProg, AccOut};
                               true -> {Input, AccOut}
                            end;
                       (_, {[], Acc}) ->
                            {[], Acc}
                    end, {Progress, Array}, combine_recipes(X1, X2)),
    Size = array:size(NewArray),
    NewElf1 = (Elf1 + X1 + 1) rem Size,
    NewElf2 = (Elf2 + X2 + 1) rem Size,
    step2(NewArray, NewElf1, NewElf2, Input, NewProg0).

sub_array(_, _, 0) ->
    [];
sub_array(_, Start, _) when Start < 0 ->
    [];
sub_array(Array, Start, N) ->
    [array:get(Start, Array)|sub_array(Array, Start + 1, N - 1)].

combine_recipes(A, B) ->
    lists:map(fun(X) -> X - $0 end, integer_to_list(A + B)).

        
%%% Tests

part1_test() ->
    ?assertEqual("0124515891", start1(5)),
    ?assertEqual("9251071085", start1(18)),
    ?assertEqual("5941429882", start1(2018)).

part2_test() ->
    ?assertEqual(9, start2(51589)),
    %% ?assertEqual(5, start2("01245")),
    ?assertEqual(18, start2(92510)),
    ?assertEqual(2018, start2(59414)).
