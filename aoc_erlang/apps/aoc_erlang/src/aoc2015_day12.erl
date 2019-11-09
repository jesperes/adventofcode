-module(aoc2015_day12).

-include_lib("eunit/include/eunit.hrl").

main_test_() ->
  Obj = jsone:decode(inputs:get_as_binary(2015, 12)),
  [ {"Part 1", fun() -> ?assertEqual(119433, count(Obj)) end}
  , {"Part 2", fun() -> ?assertEqual(68466, count_nored(Obj)) end}
  ].

count(X) when is_number(X) ->
  X;
count(X) when is_binary(X) ->
  0;
count(X) when is_map(X) ->
  maps:fold(fun(_,V,Acc) -> count(V) + Acc end, 0, X);
count(X) when is_list(X) ->
  lists:foldl(fun(V,Acc) -> count(V) + Acc end, 0, X).


count_nored(X) when is_number(X) ->
  X;
count_nored(X) when is_binary(X) ->
  0;
count_nored(X) when is_map(X) ->
  {IsRed, Sum} =
    maps:fold(fun(_,<<"red">>,{_, _Acc}) ->
                  {true, 0};
                 (_,V,{IsRed, Acc}) ->
                  {IsRed, Acc + count_nored(V)}
              end, {false, 0}, X),
  case IsRed of
    true ->
      0;
    false ->
      Sum
  end;
count_nored(X) when is_list(X) ->
  lists:foldl(fun(V,Acc) -> count_nored(V) + Acc end, 0, X).
