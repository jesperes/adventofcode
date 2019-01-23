-module(puzzle21).

-export([main/0]).
-compile([export_all]).

main() ->
    {{part1, part1()},
     {part2, part2()}}.

part1() ->
    {R1, _, _, _} = outer_loop(0, 0, 0, 0),
    R1.
    
part2() -> 
    part2_loop(0, sets:new()).

part2_loop(R1, Vals) ->
    {R1_new, _, _, _} = outer_loop(R1, 0, 0, 0),
    case sets:is_element(R1_new, Vals) of
        true ->
            R1;
        false ->
            part2_loop(R1_new, sets:add_element(R1_new, Vals))
    end.

outer_loop(R1, _R2, R3, R5) ->
    inner_loop1(8725355, R1 bor 65536, R3, R5).

inner_loop1(R1, R2, R3, _) -> 
    R5_1 = R2 band 255,
    R1_1 = ((R1 + R5_1) * 65899) band 16#ffffff,
    inner_loop2(R1_1, R2, R3, R5_1).
inner_loop2(R1, R2, R3, _) when R2 >= 256 ->
    {R3_1, R5} = inner_loop3(R1, R2, R3, 0),
    R2_1 = R5,
    inner_loop1(R1, R2_1, R3_1, R5);
inner_loop2(R1, R2, R3, R5) ->
    {R1, R2, R3, R5}.

%% returns new values for R3 and R5
inner_loop3(R1, R2, _, R5) ->
    R3 = (R5 + 1) * 256,
    if R3 > R2 ->
            {R3, R5};
       true ->
            inner_loop3(R1, R2, R3, R5 + 1)
    end.

  
