-module(aoc2015_day08).

-include_lib("eunit/include/eunit.hrl").

main_test_() ->
  List = inputs:get_as_lines(2015, 8),

  [ {"Part 1",
     fun() ->
         ?assertEqual(
            1371,
            lists:foldl(fun({LSize, MSize}, Acc) ->
                            (LSize - MSize) + Acc
                        end, 0,
                        [measure(Str) ||
                          Str <- List]))
     end}
  , {"Part 2",
     fun() ->
         ?assertEqual(
            2117,
            lists:foldl(fun({Orig, Quoted}, Acc) ->
                            Acc + (length(Quoted) - length(Orig))
                        end, 0,
                        [{Str, "\"" ++ quote(Str) ++ "\""} ||
                          Str <- List]))
     end}
  ].

measure(Str) ->
  measure(Str, before).
measure([], 'after') ->
  {0, 0};
measure([$"|Rest], before) ->
  {LSize, MSize} = measure(Rest, inside),
  {LSize + 1, MSize};
measure([$"|Rest], inside) ->
  {LSize, MSize} = measure(Rest, 'after'),
  {LSize + 1, MSize};
measure([$\\,$x,_,_|Rest], inside) ->
  %% Hexadecimal literal: \xDD
  {LSize, MSize} = measure(Rest, inside),
  {LSize + 4, MSize + 1};
measure([$\\,$"|Rest], inside) ->
  %% Quoted literal: \"
  {LSize, MSize} = measure(Rest, inside),
  {LSize + 2, MSize + 1};
measure([$\\,$\\|Rest], inside) ->
  %% Quoted backslash: \\
  {LSize, MSize} = measure(Rest, inside),
  {LSize + 2, MSize + 1};
measure([_|Rest], inside) ->
  %% Literal char
  {LSize, MSize} = measure(Rest, inside),
  {LSize + 1, MSize + 1}.



quote([]) ->
  "";
quote([$"|Rest]) ->
  [$\\, $"|quote(Rest)];
quote([$\\|Rest]) ->
  [$\\, $\\|quote(Rest)];
quote([C|Rest]) ->
  [C|quote(Rest)].
