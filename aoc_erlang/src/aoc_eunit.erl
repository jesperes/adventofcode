-module(aoc_eunit).

-include_lib("eunit/include/eunit.hrl").

-include("aoc_puzzle.hrl").

-define(TIMEOUT, 60).

-spec mktest(#aoc_puzzle{}) -> {string(), {timeout, integer(), fun()}}.
mktest(PI) ->
    M = PI#aoc_puzzle.module,
    Info = M:info(),
    Year = Info#aoc_puzzle.year,
    Day = Info#aoc_puzzle.day,

    Filename =
        lists:flatten(
            io_lib:format("~s/inputs/~w/input~2..0w.txt", ["priv", Year, Day])),
    Input =
        case Info#aoc_puzzle.has_input_file of
            true ->
                {ok, Binary} = file:read_file(Filename),
                Binary;
            false ->
                <<>>
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
                 P2 = M:solve2(ParsedInput),
                 ?assertEqual({P1, P2}, Info#aoc_puzzle.expected)
         end
      end}}.

find_changed_modules() ->
    lists:filtermap(fun(Line) ->
                       case re:run(Line,
                                   ".*/(aoc\\d+_day\\d+).erl$",
                                   [{capture, all_but_first, list}])
                       of
                           nomatch -> false;
                           {match, [ModStr]} -> {true, list_to_atom(ModStr)}
                       end
                    end,
                    string:tokens(
                        os:cmd("git status -u -s"), "\r\n")).

%% Generate a eunit test case for all the modules which implement the
%% aoc_puzzle behavior.
aoc_test_() ->
    case find_changed_modules() of
        [] ->
            %% No changed modules, run all puzzles.
            [begin
                 PuzzleInfos = aoc_puzzle:find_puzzles(Year, all),
                 {integer_to_list(Year), lists:map(fun mktest/1, PuzzleInfos)}
             end
             || Year <- lists:seq(2015, 2020)];
        ChangedModules ->
            %% Some modules have local modifications, run them only.
            PuzzleInfos = [M:info() || M <- ChangedModules],
            {"Locally edited modules", lists:map(fun mktest/1, PuzzleInfos)}
    end.
