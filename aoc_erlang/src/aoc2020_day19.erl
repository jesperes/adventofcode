%%% Advent of Code solution for 2020 day 19.
%%% Created: 2020-12-19T06:02:22+00:00

-module(aoc2020_day19).
-include_lib("eunit/include/eunit.hrl").

-compile([nowarn_unused_function, export_all, nowarn_export_all]).

part1(Input) ->
  parts(Input, none, false).

part2(Input) ->
  parts(Input, none, true).

%% Puzzle solution
parts(Input, Id, Replace) ->
  {RawRules, Messages} = parse(Input),

  Rules =
    case Replace of
      true ->
        maps:merge(#{8 => [[42], [42, 8]],
                     11 => [[42, 31], [42, 11, 31]]},
                   RawRules);
      false ->
        RawRules
    end,

  case Id of
    none -> ok;
    Suffix -> generate_dot_graphs(Rules, Suffix)
  end,

  RE = to_regex(0, Rules, none),
  {ok, CompiledRE} = re:compile("^" ++ RE ++ "$"),

  lists:foldl(fun(M, Acc) ->
                  case re:run(M, CompiledRE) of
                    {match, _} -> Acc + 1;
                    _ -> Acc
                  end
              end, 0, Messages).

%% Generate .dot+.png graphs for the rules, to easier spot the
%% differences.
generate_dot_graphs(Rules, Id) ->
  Dot = to_dot(Rules),
  Basename = io_lib:format("day19_~s", [Id]),
  ?debugFmt("Generating dot graph: ~s.dot~n", [Basename]),
  ok = file:write_file(Basename ++ ".dot", Dot),
  [] = os:cmd(lists:flatten(io_lib:format("dot -Tpng -o ~s.png ~s.dot",
                                          [Basename, Basename]))).

%% ======================================================================
%% Part 1
%% ======================================================================

to_regex(Key, RuleMap, _Parent) when is_integer(Key) ->
  case maps:get(Key, RuleMap) of
    [RHS] -> to_regex(RHS, RuleMap, Key);
    [_|_] = RHS ->
      "(" ++
        lists:join(
          "|",
          lists:map(
            fun(X) ->
                %% ?debugFmt("LHS ~w -> RHS ~w", [Key, X]),
                to_regex(X, RuleMap, Key)
            end,
            RHS)) ++ ")"
  end;
to_regex(Key, RuleMap, Parent) when is_list(Key) ->
  lists:join(
    "",
    lists:map(
      fun(X) ->
          if Parent == X ->
              throw({loop, X});
             true ->
              to_regex(X, RuleMap, Parent)
          end
      end,
      Key));
to_regex(Key, _RuleMap, _) when is_atom(Key) ->
  atom_to_list(Key).

%% ======================================================================
%% Part 2
%% ======================================================================

to_dot(RuleMap) ->
  "digraph rules {\n" ++
    to_dot0(RuleMap) ++
    "}\n".

to_dot0(RuleMap) ->
  lists:map(
    fun({LHS, RHS}) ->
        RHSNode = make_rhs_node(RHS),
        %% io:format("~p -> ~p (~s)~n", [LHS, RHS, RHSNode]),
        io_lib:format("rule_~p -> ~s;~n", [LHS, RHSNode]) ++
             lists:map(
                  fun(R) ->
                      io_lib:format("~s -> rule_~p;~n",
                                    [RHSNode, R])
                  end, RHS)
    end,
    [{LHS, RHS} || {LHS, RHSList} <- maps:to_list(RuleMap),
                   RHS <- RHSList]).

make_rhs_node(RHS) ->
  "i_" ++
    lists:join("_and_",
               lists:map(fun(R) when is_atom(R) ->
                             atom_to_list(R);
                            (R) when is_integer(R) ->
                             integer_to_list(R)
                         end, RHS)).



%% ======================================================================
%% Parser
%% ======================================================================

parse(Input) ->
  [RuleBlock, MessageBlock] = binary:split(Input, <<"\n\n">>),
  Rules = binary:split(RuleBlock, <<"\n">>, [global]),
  Messages = binary:split(MessageBlock, <<"\n">>, [global]),
  Rules0 = lists:foldl(fun parse_rule/2, #{}, Rules),
  {Rules0, Messages}.

parse_rule(Binary, Acc) ->
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
  maps:put(LHS0, SubRules, Acc).

btoi(B) ->
  case re:run(B, "\"([ab])\"|(\\d+)", [{capture, all_but_first, list}]) of
    {match, [[], RuleNum]} ->
      list_to_integer(RuleNum);
    {match, [Terminal]} ->
      list_to_atom(Terminal)
  end.

%% Input reader (place downloaded input file in
%% priv/inputs/2020/input19.txt).
get_input() ->
  inputs:get_as_binary(2020, 19).

%% Tests
main_test_() ->
  Input = get_input(),
  [ {"Part 1", ?_assertEqual(147, parts(Input, none, false))}
  %% , {"Part 2", ?_assertEqual(0, parts(Input, "part2", false))}
  ].

test_input1() ->
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
  ?_assertEqual(2, parts(test_input1(), none, false)).

test_input2() ->
  <<"42: 9 14 | 10 1\n"
    "9: 14 27 | 1 26\n"
    "10: 23 14 | 28 1\n"
    "1: \"a\"\n"
    "11: 42 31\n"
    "5: 1 14 | 15 1\n"
    "19: 14 1 | 14 14\n"
    "12: 24 14 | 19 1\n"
    "16: 15 1 | 14 14\n"
    "31: 14 17 | 1 13\n"
    "6: 14 14 | 1 14\n"
    "2: 1 24 | 14 4\n"
    "0: 8 11\n"
    "13: 14 3 | 1 12\n"
    "15: 1 | 14\n"
    "17: 14 2 | 1 7\n"
    "23: 25 1 | 22 14\n"
    "28: 16 1\n"
    "4: 1 1\n"
    "20: 14 14 | 1 15\n"
    "3: 5 14 | 16 1\n"
    "27: 1 6 | 14 18\n"
    "14: \"b\"\n"
    "21: 14 1 | 1 14\n"
    "25: 1 1 | 1 14\n"
    "22: 14 14\n"
    "8: 42\n"
    "26: 14 22 | 1 20\n"
    "18: 15 15\n"
    "7: 14 5 | 1 21\n"
    "24: 14 1\n"
    "\n"
    "abbbbbabbbaaaababbaabbbbabababbbabbbbbbabaaaa\n"
    "bbabbbbaabaabba\n"
    "babbbbaabbbbbabbbbbbaabaaabaaa\n"
    "aaabbbbbbaaaabaababaabababbabaaabbababababaaa\n"
    "bbbbbbbaaaabbbbaaabbabaaa\n"
    "bbbababbbbaaaaaaaabbababaaababaabab\n"
    "ababaaaaaabaaab\n"
    "ababaaaaabbbaba\n"
    "baabbaaaabbaaaababbaababb\n"
    "abbbbabbbbaaaababbbbbbaaaababb\n"
    "aaaaabbaabaaaaababaa\n"
    "aaaabbaaaabbaaa\n"
    "aaaabbaabbaaaaaaabbbabbbaaabbaabaaa\n"
    "babaaabbbaaabaababbaabababaaab\n"
    "aabbbbbaabbbaaaaaabbbbbababaaaaabbaaabba\n">>.

ex2_test_() ->
  [ %% ?_assertEqual(3, part2(test_input2a(), 1))
    %% ?_assertEqual(3, part2(test_input2b(), 2))
    {"Part 2 (no loops)", ?_assertEqual(3, parts(test_input2(), "testinput2_noloops", false))}
  , {"Part 2 (modified, with loops)", ?_assertEqual(3, parts(test_input2(), "testinput2_noloops", false))}
  ].

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
