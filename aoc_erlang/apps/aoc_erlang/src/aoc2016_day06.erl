-module(aoc2016_day06).
-include_lib("eunit/include/eunit.hrl").

-define(TEST_INPUT, ["eedadn", "drvtee", "eandsr", "raavrd", "atevrs",
                     "tsrnev", "sdttsa", "rasrtv", "nssdts", "ntnada",
                     "svetve", "tesnvt", "vntsnd", "vrdear", "dvrsen",
                     "enarar"]).

find_keys(Value, Map) when is_map(Map) ->
  maps:keys(maps:filter(fun(_, V) -> Value =:= V end, Map)).

get_maxmin_word(Words, Pos) ->
  FM =
    lists:foldl(fun(Word, Acc) ->
                    CharAtPos = lists:nth(Pos, Word),
                    maps:update_with(CharAtPos,
                                     fun(Old) -> Old + 1 end,
                                     1, Acc)
                end, #{}, Words),
  Freqs = maps:values(FM),
  %% Find the char corresponding to the minimum/maximum frequencies.
  %% There should only be one each of these.
  [MinChar] = find_keys(lists:min(Freqs), FM),
  [MaxChar] = find_keys(lists:max(Freqs), FM),
  {MaxChar, MinChar}.

get_message([First|_] = Words) ->
  lists:unzip(lists:map(fun(Pos) ->
                            get_maxmin_word(Words, Pos)
                        end, lists:seq(1, length(First)))).

main_test_() ->
  [ {"Test input",
     ?_assertEqual({"easter", "advent"}, get_message(?TEST_INPUT))}
  , {"Part 1 & 2",
     ?_assertEqual({"dzqckwsd", "lragovly"}, get_message(inputs:get_as_lines(2016, 6)))}
  ].
