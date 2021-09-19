-module(aoc_eunit).

-include_lib("eunit/include/eunit.hrl").

-include("aoc_puzzle.hrl").

-define(TIMEOUT, 60).

-spec mktest(#aoc_puzzle{} | integer()) ->
                {string(), {timeout, integer(), fun()}} | {string(), fun()}.
mktest(Day) when is_integer(Day) ->
    {lists:flatten(
         io_lib:format("Day ~p: -- not implemented --", [Day])),
     fun() -> ok end};
mktest(PI) ->
    M = PI#aoc_puzzle.module,
    Info = aoc_puzzle:info(M),
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
         io_lib:format("Day ~2..0w: ~s", [Day, Info#aoc_puzzle.name])),
     {timeout,
      ?TIMEOUT,
      fun() ->
         ParsedInput = M:parse(Input),
         case Info#aoc_puzzle.use_one_solver_fun of
             true ->
                 P = aoc_puzzle:solve(M, ParsedInput),
                 ?assertEqual(Info#aoc_puzzle.expected, P);
             false ->
                 P1 = aoc_puzzle:solve1(M, ParsedInput),
                 P2 = aoc_puzzle:solve2(M, ParsedInput),
                 ?assertEqual(Info#aoc_puzzle.expected, {P1, P2})
         end
      end}}.

find_changed_modules() ->
    lists:filtermap(fun(Line) ->
                       case re:run(Line,
                                   " M .*/(aoc\\d+_day\\d+).erl$",
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
    FirstYear = 2015,
    {ThisYear, ThisMonth, _} = erlang:date(),

    case find_changed_modules() of
        [] ->
            [{integer_to_list(Year),
              [begin
                   Module =
                       list_to_atom(lists:flatten(
                                        io_lib:format("aoc~w_day~2..0w", [Year, Day]))),
                   try
                       mktest(aoc_puzzle:info(Module))
                   catch
                       error:undef ->
                           mktest(Day)
                   end
               end
               || Day <- lists:seq(1, 25)]}
             || Year <- lists:seq(FirstYear, ThisYear),
                %% Start enumerating this year's puzzles on dec 1st.
                Year < ThisYear orelse ThisYear == Year andalso ThisMonth == 12];
        ChangedModules ->
            %% Some modules have local modifications, run them only.
            PuzzleInfos = [M:info() || M <- ChangedModules],
            {"Locally edited modules", lists:map(fun mktest/1, PuzzleInfos)}
    end.
