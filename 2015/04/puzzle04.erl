-module(puzzle04).
-export([start/0]).

start() ->
    Input = "yzbqklnj",
    {part1(Input, 0),
     part2(Input, 0)}.

part1(Input, N) ->
    case erlang:md5(Input ++ integer_to_list(N)) of
        <<0, 0, 0:4, _/bitstring>> -> N;
        _ -> part1(Input, N + 1)
    end.

part2(Input, N) ->
    case erlang:md5(Input ++ integer_to_list(N)) of
        <<0, 0, 0, _/binary>> -> N;
        _ -> part2(Input, N + 1)
    end.
