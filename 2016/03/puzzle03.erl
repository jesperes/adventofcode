-module(puzzle03).

-export([start/0]).

start() ->
    {ok, Bin} = file:read_file("input.txt"),
    Triangles =
        lists:map(
          fun(Line) ->
                  [A, B, C] = string:tokens(Line, " "),
                  {list_to_integer(A),
                   list_to_integer(B),
                   list_to_integer(C)}
          end, string:tokens(binary_to_list(Bin), "\n\r")),

    {count_triangles(Triangles),
     count_triangles(flip(Triangles))}.

count_triangles(L) ->
    length(lists:filter(fun is_triangle/1, L)).

is_triangle({A, B, C}) ->
    ((A + B) > C)
        and ((B + C) > A)
        and ((A + C) > B).

flip(L) ->
    flip(L, []).

flip([], Acc) -> Acc;
flip([{A1, B1, C1},
      {A2, B2, C2},
      {A3, B3, C3}|Rest], Acc) ->
    flip(Rest, [{A1, A2, A3}, {B1, B2, B3}, {C1, C2, C3}|Acc]).
