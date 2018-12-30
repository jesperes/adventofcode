%%% @author Jesper Eskilson <>
%%% @copyright (C) 2018, Jesper Eskilson
%%% @doc
%%%
%%% @end
%%% Created : 30 Dec 2018 by Jesper Eskilson <>

-module(puzzle20_part2).
-compile([export_all]).

start() ->
    timer:tc(fun() -> deliver(36000000) end).

deliver(Input) ->
    deliver(1, #{}, Input).

deliver(Elf, Houses, Input) ->
    %% Have an elf deliver all his 50 presents.
    H0 = deliver_to_houses(Elf, Elf, 1, Houses),
    
    %% The house with the same number as the elf which has just
    %% finished delivering presents will not get any more presents,
    %% since the next elf will start at "Elf + 1" delivering presents.
    %% So now we know that this house will not receive any more
    %% presents.
    N = maps:get(Elf, H0),
    if N >= Input ->
            {found, Elf, N};
       true ->
            H1 = maps:remove(Elf, H0),
            deliver(Elf + 1, H1, Input)
    end.

deliver_to_houses(_Elf, _HouseNum, NumHousesDeliveredTo, Houses) when NumHousesDeliveredTo > 50 ->
    Houses;
deliver_to_houses(Elf, HouseNum, NumHousesDeliveredTo, Houses) ->
    NumPresents = Elf * 11,

    H0 = maps:update_with(HouseNum, 
                          fun(V) -> V + NumPresents end,
                          NumPresents, Houses),
    deliver_to_houses(Elf, HouseNum + Elf, NumHousesDeliveredTo + 1, H0).
