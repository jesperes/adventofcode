#!/usr/bin/env escript 

solution(1, part1) -> 470;
solution(1, part2) -> 790;
solution(2, part1) -> 9139;
solution(2, part2) -> "uqcidadzwtnhsljvxyobmkfyr";
solution(3, part1) -> 105231;
solution(3, part2) -> 164;
solution(4, part1) -> 140932;
solution(4, part2) -> 51232;
solution(5, part1) -> 10496;
solution(5, part2) -> 5774;
solution(6, part1) -> 3894;
solution(6, part2) -> 39398;
solution(7, part1) -> "BFLNGIRUSJXEHKQPVTYOCZDWMA";
solution(7, part2) -> 880;
solution(8, part1) -> 36307;
solution(8, part2) -> 25154;
solution(9, part1) -> 423717;
solution(9, part2) -> 3553108197;
solution(10, part1) -> 10036;
solution(10, part2) -> "JJXZHKFP";
solution(11, part1) -> {235,14};
solution(11, part2) -> 1133;
solution(12, part1) -> 2767;
solution(12, part2) -> 2650000001362;
solution(13, part1) -> {94,78};
solution(13, part2) -> {26,85};
solution(_, _) -> not_implemented.

%% Goal: get all of these times down to under 5 seconds per part.
%% Day  1 solution, time =     316.84 msecs
%% Day  2 solution, time =     18.452 msecs
%% Day  3 solution, time =  39230.006 msecs
%% Day  4 solution, time =     11.366 msecs
%% Day  5 solution, time =  13203.479 msecs
%% Day  6 solution, time =   1141.034 msecs
%% Day  7 solution, time =     16.256 msecs
%% Day  8 solution, time =      8.038 msecs
%% Day  9 solution, time =   4404.032 msecs
%% Day 10 solution, time =   2335.187 msecs

main(_) ->
    Puzzles = [ 1
              , 2
              , 3
              , 4
              , 5
              , 6
              , 7
              , 8
              , 9
              , 10
                
                %% These aren't returning correct results
                %% , 11
                %% , 12
                %% , 13 
              ],
    Dir = filename:absname("."),
    {Time, _} = 
        timer:tc(fun() -> 
                         lists:foreach(fun(Day) ->
                                               run_puzzle(Dir, Day)
                                       end, Puzzles)   
                 end),
    io:format("Total time: ~w seconds (~w msecs/puzzle)~n",
              [Time / 1000000, (Time / 1000) / length(Puzzles)]).

run_puzzle(Dir, Day) ->
    SubDir = io_lib:format("~s/~w", [Dir, Day]),
    Src = io_lib:format("~s/puzzle~w.erl", [SubDir, Day]),
    file:set_cwd(SubDir),
    case compile:file(Src,
                      [nowarn_export_all, verbose, 
                       report_warnings, report_errors]) of
        {ok, Mod} ->
            Expected = 
                {{part1, solution(Day, part1)},
                 {part2, solution(Day, part2)}},
            
            {Time, Actual} = timer:tc(fun() -> 
                                              Mod:main()
                                      end),
            
            io:format("Day ~2w solution, time = ~10w msecs~n",
                      [Day, Time/1000]),
            
            if Actual == Expected ->
                    ok;
               true ->
                    io:format("Incorrect result reported by day ~w solution:~n",
                              [Day]),
                    io:format("*** EXPECTED: ~p~n", [Expected]),
                    io:format("*** ACTUAL:   ~p~n", [Actual])
            end;
        
        Other ->
            io:format("Failed to compile: ~w~n", [Other])
    end.
