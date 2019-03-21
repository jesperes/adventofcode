-module(puzzle04).
-export([start/0]).

start() ->
    part("yzbqklnj", 0, undef).

part(Input, N, P1) ->
    case erlang:md5(Input ++ integer_to_list(N)) of
        <<0, 0, 0, _/binary>> -> 
            %% Found part 2, we are done
            {P1, N};
        <<0, 0, 0:4, _/bitstring>> when P1 =:= undef -> 
            %% Found part 1 solution, continue with part 2.
            part(Input, N + 1, N);
        _ -> 
            part(Input, N + 1, P1)
    end.
