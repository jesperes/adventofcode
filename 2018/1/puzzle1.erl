-module(puzzle1).
-export([main/0]).

main() ->
    compile:file("utils.erl"),
    {{part1, start1()},
     {part2, start2()}}.

start1() ->
    Lines = utils:read_file_lines("input.txt"),
    FreqList = 
        lists:map(fun(Line) ->
                          list_to_integer(Line)
                  end, Lines),
    lists:foldl(fun(N, Acc) ->
                        N + Acc
                end, 0, FreqList).

start2() ->
    Lines = utils:read_file_lines("input.txt"),
    FreqList = 
        lists:map(fun(Line) ->
                          list_to_integer(Line)
                  end, Lines),
    find_first_duplicate(FreqList).

find_first_duplicate(IntList) ->
    find_first_duplicate_repeat(IntList, 0, sets:from_list([0])).

find_first_duplicate_repeat(IntList, Freq, Set) ->
    %% Run a pass over the set of frequencies
    case find_first_duplicate(IntList, Freq, Set) of
        %% Did we find it?
        {true, NewFreq} ->
            %% yes
            NewFreq;
        {false, LastFreq, LastSet} ->
            %% no, repeat with new state
            find_first_duplicate_repeat(IntList, LastFreq, LastSet)
    end.


find_first_duplicate([], Freq, FreqSet) ->
    %% Not found. Return the frequency and the set of seen frequencies
    %% so that we can restart.
    {false, Freq, FreqSet};
find_first_duplicate([N|IntList], Freq, FreqSet) ->
    %% Compute the next frequency
    NewFreq = N + Freq,

    %% Check if we have seen it before
    case sets:is_element(NewFreq, FreqSet) of
        true ->
            %% Yes, we are done. Return the found frequency.
            {true, NewFreq};
        false ->
            %% Nope, add the frequency to the set of seen frequencies,
            %% and continue.
            find_first_duplicate(IntList, NewFreq, 
                                 sets:add_element(NewFreq, FreqSet))
    end.
