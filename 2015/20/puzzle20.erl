%%% @author Jesper Eskilson <>
%%% @copyright (C) 2018, Jesper Eskilson
%%% @doc
%%%
%%% @end
%%% Created : 30 Dec 2018 by Jesper Eskilson <>

-module(puzzle20).
-export([start/0]).

start() ->
    {part1(),
     part2()}.

part1() ->
    Elves = 1000000,
    Limit = 36000000,
    Houses = maps:new(),
    NumHouses = Elves,

    H0 = 
        lists:foldl(fun(Elf, AccIn) ->
                            deliver1(Elf, AccIn, NumHouses)
                    end, Houses, lists:seq(1, Elves)),
    
    find_lowest_house_number(Limit, H0).

deliver1(Elf, Houses, NumHouses) ->
    lists:foldl(fun(House, AccIn) ->
                        maps:update_with(House, fun(V) ->
                                                        V + Elf * 10
                                                end, Elf * 10, AccIn)
                end, Houses, lists:seq(Elf, NumHouses, Elf)).


find_lowest_house_number(PresentLimit, Houses) ->
    find_lowest_house_number(1, PresentLimit, Houses).

find_lowest_house_number(HouseNum, PresentLimit, Houses) ->
    case maps:is_key(HouseNum, Houses) of
        true ->
            NumPresents = maps:get(HouseNum, Houses),
            if NumPresents >= PresentLimit ->
                    HouseNum;
               true ->
                    find_lowest_house_number(HouseNum + 1, PresentLimit, Houses)
            end;
        _ ->
            no_such_house
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
