-module(aoc2016_day04).

-include_lib("eunit/include/eunit.hrl").

-define(STORAGE_NAME, "northpole-object-storage").

main_test_() ->
  Lines = inputs:get_as_lines(2016, 4),
  {"Part 1 & 2",
   ?_assertEqual(
      {185371, 984},
      lists:foldl(fun(Str, {Acc0, Acc1}) ->
                      Room = room(Str),
                      {case Room of
                         {Id1, X, X, _} -> Acc0 + Id1;
                         _ -> Acc0
                       end,
                       case Room of
                         {Id2, _, _, ?STORAGE_NAME} -> Id2;
                         _ -> Acc1
                       end}
                  end, {0, undef}, Lines))}.


encrypted_name(Str) ->
  EncrName =
    lists:takewhile(
      fun(C) ->
          (C =:= $-) or (C >= $a) and (C =< $z)
      end, Str),
  lists:sublist(EncrName, 1, length(EncrName) - 1).

room(Str) ->
  Tokens = string:tokens(Str, "-[]"),
  [ChkSum, ID|_] = lists:reverse(Tokens),
  IdInt = list_to_integer(ID),
  EncrName = encrypted_name(Str),
  {IdInt,
   lists:flatten(ChkSum),
   most_common(EncrName),
   decrypt(EncrName, IdInt)}.

most_common(Str) ->
  FreqTable = freq_table(Str),
  lists:map(fun({_, K}) -> K end,
            lists:sublist(
              lists:sort(
                lists:map(fun({K, V}) ->
                              {-V, K}
                          end, maps:to_list(FreqTable))), 5)).

decrypt(Str, Key) -> decrypt(Str, Key, []).
decrypt([], _, Acc) -> lists:reverse(Acc);
decrypt([$-|Rest], Key, Acc) ->
  decrypt(Rest, Key, [$-|Acc]);
decrypt([C|Rest], Key, Acc) ->
  D = (((C - $a) + Key) rem 26) + $a,
  decrypt(Rest, Key, [D|Acc]).

freq_table(Str) -> freq_table(Str, #{}).

freq_table([], Map) -> Map;
freq_table([$-|Rest], Map) -> freq_table(Rest, Map);
freq_table([C|Rest], Map) ->
  freq_table(Rest, maps:update_with(C, fun(V) -> V + 1 end, 1, Map)).
