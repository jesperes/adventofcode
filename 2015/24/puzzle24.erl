-module(puzzle24).
-compile([export_all]).

%% https://www.geeksforgeeks.org/partition-set-k-subsets-equal-sum/

start() ->
    part3(test_data()).

part3(Packages) ->
    TotalSum = lists:sum(Packages),
    case TotalSum rem 3 of
        0 ->
            Sum = TotalSum div 3,
            io:format("Subset sum = ~w~n", [Sum]),
            part3(Packages, {Sum, Sum, Sum});
        _ ->
            io:format("Total sum ~w not divisible by 3.~n", [TotalSum]),
            []
    end.

%% Returns a list of all combinations of {ElemsA, ElemsB, ElemsC}
%% where the sum of all elements in ElemsX are the same.

part3([], {0, 0, 0}) ->
    [{[], [], []}];
part3([P|Packages], {A, B, C} = Bins) ->
    As = if P =< A -> 
                 part3(Packages, {A - P, B, C});
            true -> []
         end,

    Asols = lists:map(fun({A0, B0, C0}) ->
                              {[P|A0], B0, C0}
                      end, As),

    Bs = if P =< B -> 
                 part3(Packages, {A, B - P, C});
            true -> []
         end,

    Bsols = lists:map(fun({A1, B1, C1}) ->
                              {A1, [P|B1], C1}
                      end, Bs),

    Cs = if P =< C -> 
                 part3(Packages, {A, B, C - P});
            true -> []
         end,
    
    Csols = lists:map(fun({A2, B2, C2}) ->
                              {A2, B2, [P|C2]}
                      end, Cs),

    Asols ++ Bsols ++ Csols.


real_data() -> [1, 2, 3, 7, 11, 13, 17, 19, 23, 31, 37, 41, 43, 47,
                53, 59, 61, 67, 71, 73, 79, 83, 89, 97, 101, 103, 107,
                109, 113].

test_data() -> [1, 2, 3, 4, 5, 7, 8, 9, 10, 11].
    
