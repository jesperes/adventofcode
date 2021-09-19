-module(aoc2018_day01).
-include_lib("eunit/include/eunit.hrl").

main_test_() ->
  Input = inputs:get_as_lines(2018, 1),
  FreqList =  lists:map(fun list_to_integer/1, Input),

  [ {"Part 1", ?_assertEqual(470, lists:foldl(fun(A, B) -> A + B end, 0, FreqList))}
  , {"Part 2", ?_assertEqual(790, find_first_duplicate(FreqList))}
  ].

find_first_duplicate(IntList) ->
  find_first_duplicate_repeat(IntList, 0, sets:from_list([0])).

find_first_duplicate_repeat(IntList, Freq, Set) ->
  case find_first_duplicate(IntList, Freq, Set) of
    {true, NewFreq} -> NewFreq;
    {false, LastFreq, LastSet} ->
      find_first_duplicate_repeat(IntList, LastFreq, LastSet)
  end.

find_first_duplicate([], Freq, FreqSet) ->
  {false, Freq, FreqSet};
find_first_duplicate([N|IntList], Freq, FreqSet) ->
  NewFreq = N + Freq,
  case sets:is_element(NewFreq, FreqSet) of
    true -> {true, NewFreq};
    false -> find_first_duplicate(IntList, NewFreq,
                                  sets:add_element(NewFreq, FreqSet))
  end.
