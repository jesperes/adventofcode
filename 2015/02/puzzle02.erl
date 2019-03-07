-module(puzzle02).
-export([start/0]).

start() ->
    {ok, Bin} = file:read_file("input.txt"),
    LineFun = fun(L) ->
                      lists:map(fun list_to_integer/1, 
                                string:tokens(L, "x"))
              end,

    lists:foldl(
      fun(Line, {A1, A2}) ->
              [X, Y, Z] = LineFun(Line),
              {A1 + 2*X*Y + 2*Y*Z + 2*Z*X + lists:min([X*Y, Y*Z, Z*X]),
               A2 + 2*lists:min([X+Y, Y+Z, Z+X]) + X*Y*Z}
      end, {0, 0}, string:tokens(binary_to_list(Bin), "\n\r")).
