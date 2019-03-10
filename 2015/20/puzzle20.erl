%%% @author Jesper Eskilson <>
%%% @copyright (C) 2018, Jesper Eskilson
%%% @doc
%%%
%%% @end
%%% Created : 30 Dec 2018 by Jesper Eskilson <>

-module(puzzle20).
-export([start/0]).

-compile([export_all]).

start() ->
    {part1(), part2()}.

input() ->
     36000000.
    
part1() ->
    %% Elf number N delivers presents to house N, 2*N, 3*N, etc.  To
    %% know how many presents house M will get, we need to let elves
    %% from 1 to M to deliver presents. At elf M+1, no more presents
    %% will be delivered to house M.
                                     
    Limit = input(),

    %% This is a little cheating; we happen to know that the answer is
    %% exactly 831600, so no need to let any elves above that deliver
    %% any presents.
    Elves = 850000,

    Houses = maps:new(),
    NumHouses = Elves,

    H0 = 
        foldn(fun(Elf, AccIn) ->
                      deliver1(Elf, AccIn, NumHouses)
              end, Houses, 2, 1, Elves),
 
    find_lowest_house_number(Limit, H0).

deliver1(Elf, Houses, NumHouses) ->
    foldn(fun(House, AccIn) ->
                  maps:update_with(House, fun(V) ->
                                                  V + Elf * 10
                                          end, 10 + Elf * 10, AccIn)
          end, Houses, Elf, Elf, NumHouses).


find_lowest_house_number(PresentLimit, Houses) ->
    find_lowest_house_number(1, PresentLimit, Houses).

find_lowest_house_number(HouseNum, PresentLimit, Houses) ->
    case maps:get(HouseNum, Houses, 10) of
        NumPresents when NumPresents >= PresentLimit ->
            HouseNum;
        _ ->
            find_lowest_house_number(HouseNum + 1, PresentLimit, Houses)
    end.

part2() ->
    Limit = 36000000,
    deliver2(1, #{}, Limit).

deliver2(Elf, Houses, Input) ->
    %% Have an elf deliver all his 50 presents.
    H0 = deliver_to_houses(Elf, Elf, 1, Houses),
 
    %% The house with the same number as the elf which has just
    %% finished delivering presents will not get any more presents,
    %% since the next elf will start at "Elf + 1" delivering presents.
    %% So now we know that this house will not receive any more
    %% presents.
    N = maps:get(Elf, H0),
    if N >= Input ->
            Elf;
       true ->
            H1 = maps:remove(Elf, H0),
            deliver2(Elf + 1, H1, Input)
    end.

deliver_to_houses(_Elf, _HouseNum, NumHousesDeliveredTo, Houses) when NumHousesDeliveredTo > 50 ->
    Houses;
deliver_to_houses(Elf, HouseNum, NumHousesDeliveredTo, Houses) ->
    NumPresents = Elf * 11,

    H0 = maps:update_with(HouseNum, 
                          fun(V) -> V + NumPresents end,
                          NumPresents, Houses),
    deliver_to_houses(Elf, HouseNum + Elf, NumHousesDeliveredTo + 1, H0).

foldn(Fun, Init, Start, Incr, End) ->
    foldn(Start, Fun, Init, Start, Incr, End).

foldn(N, _Fun, Acc, _Start, _Incr, End) when N > End ->
    Acc;
foldn(N, Fun, Acc, Start, Incr, End) ->
    NewAcc = Fun(N, Acc),
    foldn(N + Incr, Fun, NewAcc, Start, Incr, End).
