%%% Advent of Code solution for 2020 day 19.
%%% Created: 2020-12-19T06:02:22+00:00

-module(aoc2020_day19).
-include_lib("eunit/include/eunit.hrl").

-compile([nowarn_unused_function, export_all, nowarn_export_all]).

%% Puzzle solution
part1(Input) ->
  %% redbug:start("aoc2020_day19:reduce"),
  [RuleBlock, MessageBlock] = binary:split(Input, <<"\n\n">>),
  Rules = binary:split(RuleBlock, <<"\n">>, [global]),
  Messages = binary:split(MessageBlock, <<"\n">>, [global]),
  Messages0 =
    lists:map(
      fun(M) ->
          lists:map(
            fun(C) -> list_to_atom([C]) end, binary_to_list(M))
      end, Messages),

  RuleMap =
    lists:foldl(
      fun(Rule, Acc) ->
          parse_rule(Rule, Acc)
      end, #{}, Rules),

  reduce(hd(Messages0), RuleMap).

parse_rule(Binary, Map) ->
  [LHS, RHS] = binary:split(Binary, <<":">>),
  SubRules = lists:map(
               fun(B) ->
                   lists:filtermap(
                     fun(B0) ->
                         case byte_size(B0) > 0 of
                           true -> {true, btoi(B0)};
                           false -> false
                         end
                     end,
                     binary:split(B, <<" ">>, [global]))
               end, binary:split(RHS, <<"|">>, [global])),
  LHS0 = btoi(LHS),
  maps:merge(
    Map,
    maps:from_list([{SubRule, LHS0} || SubRule <- SubRules])).

btoi(B) ->
  case re:run(B, "\"([ab])\"|(\\d+)", [{capture, all_but_first, list}]) of
    {match, [[], RuleNum]} ->
      list_to_integer(RuleNum);
    {match, [Terminal]} ->
      list_to_atom(Terminal)
  end.

reduce([], Rules) ->
  {true, {unused_rules, Rules}};
reduce(Msg, Rules) when map_size(Rules) == 0 ->
  {false, {remaining_msg, Msg}};
reduce(Msg, RuleMap) ->
  io:format("Reducing ~p using rules ~p~n", [Msg, RuleMap]),
  {Rule, RuleMap0} = find_matching_rule(Msg, RuleMap),
  Msg0 = apply_rule(Rule, Msg),
  reduce(Msg0, RuleMap0).

apply_rule({LHS, RHS}, Msg) ->
  true = lists:prefix(RHS, Msg),
  {_, Msg0} = lists:split(length(RHS), Msg),
  io:format("Applied rule (~p -> ~p) on ~p to get ~p~n", [LHS, RHS, Msg, Msg0]),
  Msg0.

find_matching_rule([], _RuleMap) ->
  true;
find_matching_rule(Msg, RuleMap) ->
  case lists:filter(fun(RHS) ->
                        lists:prefix(RHS, Msg)
                    end, maps:keys(RuleMap)) of
    [RHS|_] ->
      LHS = maps:get(RHS, RuleMap),
      io:format("Msg ~p matches rule ~p -> ~p~n", [Msg, LHS, RHS]),
      {{LHS, RHS}, maps:remove(RHS, RuleMap)};
    [] ->
      io:format("Msg ~p does not match any rule~n", [Msg])
  end.

%% is_matching_rule(Msg, {_, RHS} = Rule) ->
%%   lists:prefix(RHS, Msg).

part2(_Input) ->
  ?debugMsg("Not implemented."),
  0.

%% Input reader (place downloaded input file in
%% priv/inputs/2020/input19.txt).
get_input() ->
  inputs:get_as_binary(2020, 19).

%% Tests
main_test_() ->
  _Input = get_input(),
  [].
  %% [ {"Part 1", ?_assertEqual(0, part1(Input))}
  %% , {"Part 2", ?_assertEqual(0, part2(Input))}
  %% ].

test_input() ->
  <<"0: 4 1 5\n"
    "1: 2 3 | 3 2\n"
    "2: 4 4 | 5 5\n"
    "3: 4 5 | 5 4\n"
    "4: \"a\"\n"
    "5: \"b\"\n"
    "\n"
    "ababbb\n"
    "bababa\n"
    "abbbab\n"
    "aaabbb\n"
    "aaaabbb">>.

ex1_test_() ->
  ?_assertEqual(0, part1(test_input())).


%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
