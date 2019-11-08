-module(aoc2015_day02).
-include_lib("eunit/include/eunit.hrl").

main_test() ->
  Bin = inputs:get_as_binary(2015, 2),
  LineFun = fun(L) ->
                lists:map(fun list_to_integer/1,
                          string:tokens(L, "x"))
            end,

  {1586300, 3737498} =
    lists:foldl(
      fun(Line, {A1, A2}) ->
          [X, Y, Z] = LineFun(Line),
          {A1 + 2*X*Y + 2*Y*Z + 2*Z*X + lists:min([X*Y, Y*Z, Z*X]),
           A2 + 2*lists:min([X+Y, Y+Z, Z+X]) + X*Y*Z}
      end, {0, 0}, string:tokens(binary_to_list(Bin), "\n\r")).
