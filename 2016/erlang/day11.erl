-module(day11).
-compile([export_all]).
-include_lib("eunit/include/eunit.hrl").


-define(TEST_INPUT, [
                     {1, [e, {chip, h}, {chip, l}]},
                     {2, [{gen, h}]},
                     {3, [{gen, l}]},
                     {4, []}
                    ]).

is_end_state([]) ->
    true;
is_end_state([{N, []}|Rest]) when N /= 4 ->
    is_end_state(Rest);
is_end_state([{4, _}|Rest]) ->
    is_end_state(Rest);
is_end_state(_) ->
    false.

is_end_state_test() ->
    ?assertNot(is_end_state(?TEST_INPUT)),
    ?assert(is_end_state([{4, [e, {chip, h}, {gen, l}, {gen, h}, {chip, l}]},
                          {3, []},
                          {2, []},
                          {1, []}])).


%%%
%%% Rules: 
%%% 
%%% * Elevator can carry at most two items
%%% * Elevator must have at least one RTG or microchip in it to move.
%%% * Elevator always moves one floor at a time, and is exposed to
%%%   all items on the floor it is on.
%%% * Microchips are shielded from other RTGs when and only when they
%%%   are connected to their generator.

contains_unshielded_chip([], _) ->          
    false;
contains_unshielded_chip([{chip, X}|Items], AllItems) ->
    case lists:member({gen, X}, AllItems) of
        true ->
            contains_unshielded_chip(Items, AllItems);
        false ->
            true
    end;
contains_unshielded_chip([_|Items], AllItems) ->
    contains_unshielded_chip(Items, AllItems).
            
contains_unshielded_chip(Items) ->
    contains_unshielded_chip(Items, Items).

contains_unshielded_chip_test() ->
    ?assert(contains_unshielded_chip([{chip, a}, {chip, b}, {gen, a}])),
    ?assertNot(contains_unshielded_chip([{chip, a}, {gen, b}, {chip, b}, {gen, a}])).

is_valid_state([]) ->
    true;
is_valid_state([{Floor, Items}|Rest]) ->
    contains_unshielded_chip(Items),
    is_valid_state(Rest).
    

get_all_moves_from_elevator_floor(Components) ->
    [[X] || X <- Components, X /= e] ++ 
        [[X,Y] || X <- Components, Y <- Components, X < Y, X /= e, Y /= e].

get_all_moves_from_elevator_floor_test() ->
    ?assertEqual([[a], [b], [c], [a, b], [a, c], [b, c]], 
                 get_all_moves_from_elevator_floor([a, b, c])).

%%% Returns all possible ways to move chips/generators between
%%% floors.
get_all_moves([]) ->                  
    [];
get_all_moves([{Floor, Items}|Rest]) -> 
    case lists:member(e, Items) of
        false ->
            %%% elevator is not at this floor
            [];
        true ->
            AllMoves = get_all_moves_from_elevator_floor(Items),
            AllMovesWithDest = 
                lists:flatten(lists:map(fun(Move) ->
                                                [{{to_floor, Floor - 1}, Move},
                                                 {{to_floor, Floor + 1}, Move}]
                                        end, AllMoves)),
            
            ?debugFmt("All moves: ~w~n", [AllMovesWithDest]),
            AllValidMovesWithDest = 
                lists:filter(fun(Move) ->
                                     {{to_floor, F}, _} = Move,
                                     (F =< 4) and (F >= 1)
                             end, AllMovesWithDest)
    end.

get_all_moves_test() ->
    ?assertEqual([
                  {{to_floor, 2}, [{chip, h}]},
                  {{to_floor, 2}, [{chip, l}]},
                  {{to_floor, 2}, [{chip, h}, {chip, l}]}], get_all_moves(?TEST_INPUT)).

            
    
