%%% Advent of Code solution for 2019 day 25.
%%% Created: 2019-12-25T14:48:53+00:00

-module(aoc2019_day25).
-include_lib("eunit/include/eunit.hrl").

combinations([]) -> [];
combinations([H | T]) ->
  CT = combinations(T),
  [[H]] ++ [[H | L] || L <- CT] ++ CT.

part1(Prog) ->
  %% First, I explored the maze by hand and keeping a hand-drawn map.
  %% The following items can be collected (for my input, that is):

  Items = ["fuel cell",
           "space heater",
           "hologram",
           "space law space brochure",
           "food ration",
           "tambourine",
           "spool of cat6",
           "festive hat"],

  Combinations = combinations(Items),

  %% These are the instructions needed to go fetch all items and bring
  %% them to the security checkpoing.
  BaseInstructions =
    lists:join(
      "\n",
      [ "west" %% kitchen
      , "take hologram"
      , "north" %% stables
      , "take space heater"
      , "east" %% storage
      , "take space law space brochure"
      , "east" %% passages
      , "take tambourine"
      , "west" %% storage
      , "west" %% stables
      , "south" %% kitchen
      , "east" %% hull breach
      , "east" %% crew quarters
      , "take festive hat"
      , "east" %% observatory
      , "take food ration"
      , "east" %% engineering
      , "take spool of cat6"
      , "west" %% observatory
      , "west" %% crew quarters
      , "south" %% arcade
      , "east" %% gift wrapping"
      , "east" %% sick bay
      , "east" %% security checkpoint
      ]) ++ "\n",

  %% Construct the input to the program which will collect all the
  %% items, then try all combinations of them until we find the right
  %% one.
  Instrs =
    lists:foldl(
      fun(Combination, Acc) ->
          %% We could, of course, optimize a tiny bit here to avoid
          %% dropping the items we are going to pick up, but the
          %% entire program runs in < 1s anyway...
          Acc ++ lists:join(
                   "\n",
                   %% Drop all items we might be holding
                   [ "drop spool of cat6"
                   , "drop food ration"
                   , "drop festive hat"
                   , "drop tambourine"
                   , "drop space law space brochure"
                   , "drop space heater"
                   , "drop hologram"
                   ]) ++ "\n" ++
            %% Pick up the items for this combination
            lists:map(fun(Item) ->
                          "take " ++ Item ++ "\n"
                      end, Combination) ++

            %% Uncomment this to show the inventory, but it is not
            %% necessary when solving.

            %% "inv\n"

            %% Attempt to go through the security checkpoing. When we
            %% have the right combination of items, the program will
            %% terminate, at which point it will likely not have
            %% processed all the inputs.

            "south\n"
      end, BaseInstructions, Combinations),

  %% Eventually we will find the combination
  %% - space heater
  %% - hologram
  %% - space law space brochure
  %% - spool of cat6

  %% Run the instructions
  {_, R} = intcode:execute(Prog, Instrs),

  %% Locate the keycode in the program output
  {match, [Code]} =
    re:run(lists:reverse(R),
           ".*You should be able to get in by typing (\\d+) on the keypad.*",
           [{capture, all_but_first, list}]),
  list_to_integer(Code).

%% Input reader (place downloaded input file in
%% priv/inputs/2019/input25.txt).
get_input() ->
  intcode:parse(inputs:get_as_binary(2019, 25)).

%% Tests
main_test_() ->
  Input = get_input(),
  {"Part 1", ?_assertEqual(2098048, part1(Input))}.

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
