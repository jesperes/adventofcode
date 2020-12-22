%% Behavior module for AoC puzzles

-module(aoc_puzzle).

-include("aoc_puzzle.hrl").

-export([ report/2
        , find_puzzles/2
        ]).

%% Callback to parse the input data
-callback parse(Input :: binary()) -> ParsedInput :: term().

%% Callback to solve the puzzle
-callback solve(ParsedInput :: term()) -> ok.

%% Callback to get info about a puzzle
-callback info() -> aoc_puzzle().

-spec report(Part :: part1 | part2,
             Result :: term()) -> ok.
report(_Part, _Result) ->
  ok.

-spec find_puzzles(Year :: integer() | all,
                   Day :: integer() | all) ->
        [Puzzles :: aoc_puzzle()].
find_puzzles(Year, Day) ->
  {{ThisYear, _, _}, _} = erlang:localtime(),
  lists:filtermap(
    fun({M, _Y, _D}) ->
        try
          ModInfo = M:module_info(),
          Attrs = proplists:get_value(attributes, ModInfo),
          Behaviors = proplists:get_value(behavior, Attrs),
          case Behaviors =/= undefined andalso
            lists:member(aoc_puzzle, Behaviors) of
            true -> {true, M:info()};
            false -> false
          end
        catch error:undef ->
            false
        end
    end,
    [{list_to_atom(lists:flatten(io_lib:format("aoc~w_day~2..0w", [Y, D]))), Y, D} ||
      Y <- case Year of
             all -> lists:seq(2015, ThisYear);
             Y when is_integer(Y) -> [Y]
           end,
      D <- case Day of
             all -> lists:seq(1, 25);
             D when is_integer(D) -> [D]
           end]).
