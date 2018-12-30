%%% @author Jesper Eskilson <>
%%% @copyright (C) 2018, Jesper Eskilson
%%% @doc
%%%
%%% @end
%%% Created : 30 Dec 2018 by Jesper Eskilson <>

-module(puzzle20).
-compile([export_all]).

start() ->
    Elves = 1000000,
    start(Elves, 36000000).

start(Elves, Limit) ->
    Houses = maps:new(),
    NumHouses = Elves,

    H0 = 
        lists:foldl(fun(Elf, AccIn) ->
                            deliver(Elf, AccIn, NumHouses)
                    end, Houses, lists:seq(1, Elves)),
    
    HouseNr = find_lowest_house_number(Limit, H0),

    io:format("Number of elves = ~p, first house getting ~p presents is house number ~p~n", 
              [Elves, Limit, HouseNr]).

deliver(Elf, Houses, NumHouses) ->
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
