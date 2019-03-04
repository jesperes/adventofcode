-module(puzzle05).
-export([start/0]).

start() ->
    {ok, Bin} = file:read_file("input.txt"),
    Words = string:tokens(binary_to_list(Bin), "\n"),
    {part1(Words), part2(Words)}.

part1(Words) ->
    length(lists:filter(fun is_nice_string/1, Words)).
    
part2(Words) ->
    length(lists:filter(fun is_really_nice_string/1, Words)).
    
is_nice_string(S) ->
    at_least_three_vowels(S) andalso
        twice_in_a_row(S) andalso
        no_ugly_pairs(S).

at_least_three_vowels(S) ->
    length(lists:filter(fun is_vowel/1, S)) >= 3.

twice_in_a_row([]) -> false;
twice_in_a_row([X, X|_]) -> true;
twice_in_a_row([_|L]) -> twice_in_a_row(L).
    
no_ugly_pairs(L) ->
    lists:all(fun(S) ->
                      string:find(L, S) =:= nomatch
              end, ["ab", "cd", "pq", "xy"]).

is_vowel($a) -> true;
is_vowel($e) -> true;
is_vowel($i) -> true;
is_vowel($o) -> true;
is_vowel($u) -> true;
is_vowel(_) -> false.
        
is_really_nice_string(S) ->
    has_pair_with_letter_between(S) andalso
        has_non_overlapping_pair(S).

has_pair_with_letter_between([]) -> false;
has_pair_with_letter_between([X,_,X|_]) -> true;
has_pair_with_letter_between([_|Rest]) ->
    has_pair_with_letter_between(Rest).
    
has_non_overlapping_pair(S) ->
    has_non_overlapping_pair(S, 0, #{}).

has_non_overlapping_pair([], _, _) -> false;
has_non_overlapping_pair([_], _, _) -> false;
has_non_overlapping_pair([X,Y|Rest], N, Map) ->
    %% The map keeps track of the position of the first occurrence of
    %% each pair we have seen, so when we see the same pair again at
    %% least 2 chars apart, we know we have two non-overlapping pairs.
    case maps:get({X, Y}, Map, undefined) of
        Pos when is_integer(Pos) and ((N - Pos) >= 2) ->
            true;
        undefined ->
            Map0 = maps:put({X, Y}, N, Map),
            has_non_overlapping_pair([Y|Rest], N + 1, Map0);
        _ ->
            has_non_overlapping_pair([Y|Rest], N + 1, Map)
    end.


     
               

        
                         
