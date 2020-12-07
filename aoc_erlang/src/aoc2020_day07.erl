%%% Advent of Code solution for 2020 day 07.
%%% Created: 2020-12-07T05:38:13+00:00

-module(aoc2020_day07).
-include_lib("eunit/include/eunit.hrl").

part1(Input) ->
  Bags = parse_input1(Input),
  Reachable = digraph_utils:reachable(['shiny gold'], Bags),
  length(Reachable) - 1. %% exclude the 'shiny gold' bag itself

part2(Input) ->
  num_bags('shiny gold', parse_input2(Input)).

%% For part 2, we just do a simple depth-first traversal of (part of)
%% the graph.
num_bags(Bag, Bags) ->
  Contents = maps:get(Bag, Bags, []),
  lists:foldl(
    fun({N, Color}, Acc) ->
        N * (1 + num_bags(Color, Bags)) + Acc
    end, 0, Contents).

%% ===============================================================
%% Input parser
%% ===============================================================

%% TODO optimize solution by just doing this part once for both
%% solutions.
parse_input(Lines, Init, Fun) ->
  lists:foldl(
    fun(Line, Acc) ->
        [BagColor, Contents] = string:split(Line, " bags contain "),
        ContentList = lists:map(fun string:trim/1,
                                string:lexemes(Contents, ",.")),
        ColorList =
          lists:flatten(
            lists:map(
              fun(Content) ->
                  case string:lexemes(Content, " ") of
                    [N, C1, C2, _] ->
                      {list_to_integer(N), list_to_atom(C1 ++ " " ++ C2)};
                    ["no", "other", "bags"] ->
                      []
                  end
              end, ContentList)),

        Fun(list_to_atom(BagColor), ColorList, Acc)
    end, Init, Lines).

%% Parse input into a OTP digraph for part 1
parse_input1(Lines) ->
  Graph = digraph:new(),
  parse_input(
    Lines, ok,
    fun(BagColor, Colors, _) ->
        BagV = digraph:add_vertex(Graph, BagColor),
        lists:foreach(
          fun({_, C}) ->
              CV = digraph:add_vertex(Graph, C),
              digraph:add_edge(Graph, CV, BagV)
          end, Colors)
    end),
  Graph.

%% Parse input into a plain map for part 2.
parse_input2(Lines) ->
  parse_input(
    Lines, #{},
    fun(BagColor, Colors, Acc) ->
        lists:foldl(
          fun(C, InnerAcc) ->
              maps:update_with(
                BagColor, fun(Old) -> [C|Old] end,
                [C], InnerAcc)
          end, Acc, Colors)
    end).

%% Input reader (place downloaded input file in
%% priv/inputs/2020/input07.txt).
get_input() ->
  inputs:get_as_lines(2020, 07).

%% Tests
main_test_() ->
  Input = get_input(),

  [ {"Part 1", ?_assertEqual(355, part1(Input))}
  , {"Part 2", ?_assertEqual(5312, part2(Input))}
  ].

%% Examples

part1_test_() ->
  ?_assertEqual(4, part1(test_input())).

part2_test_() ->
  ?_assertEqual(32, part2(test_input())).

test_input() ->
  ["light red bags contain 1 bright white bag, 2 muted yellow bags.",
   "dark orange bags contain 3 bright white bags, 4 muted yellow bags.",
   "bright white bags contain 1 shiny gold bag.",
   "muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.",
   "shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.",
   "dark olive bags contain 3 faded blue bags, 4 dotted black bags.",
   "vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.",
   "faded blue bags contain no other bags.",
   "dotted black bags contain no other bags."].


%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
