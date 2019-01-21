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
solution(11, part2) -> {237,227,14};
solution(12, part1) -> 2767;
solution(12, part2) -> 2650000001362;
solution(13, part1) -> {94,78};
solution(13, part2) -> {26,85};
solution(14, part1) -> 5115114101;
solution(14, part2) -> 633601;
solution(15, part1) -> 237996;
solution(15, part2) -> 69700;
solution(16, part1) -> 521;
solution(16, part2) -> 594;
solution(17, part1) -> 34775;
solution(17, part2) -> 27086;
solution(18, part1) -> 466312;
solution(18, part2) -> 176782;
solution(19, part1) -> 1500;
solution(19, part2) -> 18869760;
solution(20, part1) -> 3218;
solution(20, part2) -> 8725;
solution(21, part1) -> 4682012;
solution(21, part2) -> 5363733;
solution(22, part1) -> 6256;
solution(22, part2) -> 973;
solution(23, part1) -> 889;
solution(23, part2) -> 160646364;
solution(24, part1) -> 25088;
solution(24, part2) -> 2002;
solution(25, part1) -> 318;
solution(25, part2) -> ok.                      %% no part 2 for day 25


%% All puzzles run cleanly in less than 5 seconds, except these ones:

%% Day 14 (Chocolate Carts) is not implemented yet
%% Day 15 (Beverage Bandits) was only solved java
%% Day 19 (Go With The Flow) times out
%% Day 21 (Chronal Conversion) ??
%% Day 22 (Mode Maze) times out
%% Day 23 (Experimental Emergency Teleportation) cheated with python solution
%% Day 24 (Immune System Simulator) timeout
%% Day 25 (Four-Dimensional Adventure)

%% Total time: 36.41 seconds (1456.28 msecs/puzzle)
%% Failed:    2
%% Timeouts:  5
%% Skipped:   2
%% Succeeded: 16

timeout(_) -> timer:seconds(5).

count(What, Result) ->
    length(lists:filter(fun({W, _}) when W == What -> true;
                           (_) -> false
                        end, Result)).

main(_) ->
    Puzzles = lists:seq(1, 25),
    Dir = filename:absname("."),
    
    %% compile utility code
    Utils = 
	filelib:fold_files(Dir ++ "/../utils/erlang", ".*\\.erl$", false,
			   fun(F, AccIn) ->
				   [compile:file(F)|AccIn]
			   end, []),

    io:format("Compile utility code: ~w~n", [Utils]),

    {Time, Result} =
        timer:tc(fun() ->
                         lists:map(fun(Day) ->
                                               run_puzzle(Dir, Day)
                                       end, Puzzles)
                 end),
    io:format("Total time: ~.2f seconds (~.2f msecs/puzzle)~n",
              [Time / 1000000, (Time / 1000) / length(Puzzles)]),

    io:format("Failed:    ~w~n", [count(fail, Result)]),
    io:format("Timeouts:  ~w~n", [count(timeout, Result)]),
    io:format("Skipped:   ~w~n", [count(skipped, Result)]),
    io:format("Succeeded: ~w~n", [count(ok, Result)]).


run_puzzle(Dir, Day) ->
    SubDir = io_lib:format("~s/~w", [Dir, Day]),
    Src = io_lib:format("~s/puzzle~w.erl", [SubDir, Day]),
    file:set_cwd(SubDir),

    case filelib:is_file(Src) of
        false ->
            io:format("Day ~2w: --- Skipped, no source found ---~n", [Day]),
            {skipped, Day};
        true ->
            run_puzzle0(Src, Day)
    end.

get_expected_solution(Day) ->
    {{part1, solution(Day, part1)},
     {part2, solution(Day, part2)}}.

has_main(Mod) ->
    lists:member({main, 0}, proplists:get_value(exports, Mod:module_info())).

run_puzzle0(Src, Day) ->
    case compile:file(Src,
                      [nowarn_export_all,
                       %% nowarn_unused_function,
                       verbose,
                       %% native,
                       report_warnings,
                       report_errors]) of
        {ok, Mod} ->
            io:format("Day ~2w: ", [Day]),

            case {has_main(Mod), get_expected_solution(Day)} of
                {false, _} ->
                    io:format("--- Entry point ~w:main/0 not defined, skipping~n", [Mod]),
                    {skipped, Day};

                {_, not_implemented} ->
                    io:format("--- Solution not known, skipping~n", []),
                    {skipped, Day};

                {true, Expected} ->
                    Timeout = timeout(Day),
                    Parent = self(),
                    Pid = spawn(fun() ->
                                        Parent ! {result, timer:tc(fun() -> Mod:main() end)}
                                end),
                    receive
                        {result, {Time, Actual}} ->
                            if Actual == Expected ->
                                    io:format("OK ~10w msecs~n", [floor(Time/1000)]),
                                    {ok, Day};
                               true ->
                                    io:format("*** FAIL *** (incorrect result after ~w msecs) (expected ~w, got ~w)~n",
                                              [floor(Time/1000), Expected, Actual]),
                                    {fail, Day}
                            end;
                        Other ->
                            io:format("Msg: ~p~n", [Other])
                    after Timeout ->
                            exit(Pid, timeout),
                            io:format("*** TIMEOUT (after ~w seconds) ***~n", [floor(Timeout/1000)]),
                            {timeout, Day}
                    end;
                
                Other ->
                    io:format("Failed to compile: ~w~n", [Other]),
                    {skipped, Day}
            end
    end.
