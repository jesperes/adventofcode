-module(aoc_eunit).

-include_lib("eunit/include/eunit.hrl").

-include("aoc_puzzle.hrl").

-define(TIMEOUT, 10).

%% Generate a eunit test case for all the modules which implement the
%% aoc_puzzle behavior.
aoc_test_() ->
    [begin
         PuzzleInfos = aoc_puzzle:find_puzzles(Year, all),
         {integer_to_list(Year),
          lists:map(fun(PI) ->
                       M = PI#aoc_puzzle.module,
                       Info = M:info(),
                       Day = Info#aoc_puzzle.day,
                       Filename =
                           lists:flatten(
                               io_lib:format("~s/inputs/~w/input~2..0w.txt", ["priv", Year, Day])),
                       Input =
                           case Info#aoc_puzzle.has_input_file of
                               true ->
                                   {ok, Binary} = file:read_file(Filename),
                                   Binary;
                               false -> <<>>
                           end,

                       {lists:flatten(
                            io_lib:format("Day ~p: ~s", [Day, Info#aoc_puzzle.name])),
                        {timeout,
                         ?TIMEOUT,
                         fun() ->
                            ParsedInput = M:parse(Input),
                            case Info#aoc_puzzle.use_one_solver_fun of
                                true ->
                                    P = M:solve(ParsedInput),
                                    ?assertEqual(P, Info#aoc_puzzle.expected);
                                false ->
                                    P1 = M:solve1(ParsedInput),
                                    case Info#aoc_puzzle.day of
                                        25 -> ?assertMatch({P1, _}, Info#aoc_puzzle.expected);
                                        _ ->
                                            P2 = M:solve2(ParsedInput),
                                            ?assertEqual({P1, P2}, Info#aoc_puzzle.expected)
                                    end
                            end
                         end}}
                    end,
                    PuzzleInfos)}
     end
     || Year <- lists:seq(2015, 2020)].
