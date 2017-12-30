-module(day06).
-compile([export_all]).
-include_lib("eunit/include/eunit.hrl").

-define(SMALL_INPUT, [eedadn,
		      drvtee,
		      eandsr,
		      raavrd,
		      atevrs,
		      tsrnev,
		      sdttsa,
		      rasrtv,
		      nssdts,
		      ntnada,
		      svetve,
		      tesnvt,
		      vntsnd,
		      vrdear,
		      dvrsen,
		      enarar]).


input_as_strings() ->
    lists:map(fun(Atom) -> 
		      atom_to_list(Atom) end, 
	      ?SMALL_INPUT).

%%% Acc is a map integer positions to frequency maps for that
%%% position. This is used in a foldr() call to count the letter
%%% frequency for each position.
count_letters([], _, Acc) ->    
    Acc;
count_letters([X|Xs], N, Acc) -> 
    {Acc0, FreqMap} = case maps:is_key(N, Acc) of
			  true ->
			      %% Get the existing frequency map
			      {Acc, maps:get(N, Acc)};
			  false ->
			      %% Create new frequency map
			      NewMap = maps:new(),
			      NewAcc = maps:put(N, NewMap, Acc),
			      {NewAcc, NewMap}
		      end,
    
    %% Update the frequency map
    NewFreqMap = maps:put(X, maps:get(X, FreqMap, 0) + 1, FreqMap),

    %% Recurse with the updated map.
    count_letters(Xs, N + 1, maps:put(N, NewFreqMap, Acc0)).

get_most_frequent_letter(Map) ->
    [{Letter, _}|_] = lists:keysort(2, maps:to_list(Map)),
    %% [{Letter, _}|_] = lists:reverse(lists:keysort(2, maps:to_list(Map))),
    Letter.

get_most_frequent_letter_test() ->
    Map0 = maps:put($a, 42, maps:new()),
    Map1 = maps:put($b, 43, Map0),
    $a = get_most_frequent_letter(Map1).

%%% Convert the frequency map to a word using the most frequently used
%%% letters for each position.
frequency_map_to_letters(MapOfFreqMaps) ->
    %% Take all the frequency maps and sort them by their position
    ListOfFreqMaps = lists:keysort(1, maps:to_list(MapOfFreqMaps)),

    %% Take list of frequency map and take the most frequency frequent
    %% letter for each position
    lists:map(fun({_, Map}) ->
		      get_most_frequent_letter(Map)
	      end, ListOfFreqMaps).


get_error_corrected_message(Words) ->
    Map = lists:foldl(fun(Word, Acc) ->
			      count_letters(Word, 0, Acc)
		      end,
		      maps:new(), Words),
    frequency_map_to_letters(Map).

small_input_test() ->
    Words = input_as_strings(),
    %% "easter" = get_error_corrected_message(Words),
    "advent" = get_error_corrected_message(Words).

large_input_test() ->
    Words = utils:read_file_lines("input06.txt"),
    ?debugMsg(get_error_corrected_message(Words)).



