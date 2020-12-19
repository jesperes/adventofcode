%%% Advent of Code solution for 2020 day 19.
%%% Created: 2020-12-19T06:02:22+00:00

-module(aoc2020_day19).
-include_lib("eunit/include/eunit.hrl").

-compile([nowarn_unused_function]).

%% ======================================================================
%% Part 1
%% ======================================================================

part1(Input) ->
  {Rules, Messages} = parse(Input),
  RE = to_regex(0, Rules),
  {ok, CompiledRE} = re:compile("^" ++ RE ++ "$"),
  lists:foldl(fun(M, Acc) ->
                  case re:run(M, CompiledRE) of
                    {match, _} -> Acc + 1;
                    _ -> Acc
                  end
              end, 0, Messages).


%% ======================================================================
%% Part 2
%%
%% The input rules are modified so that they contain loops. The
%% modified rules are such that
%%
%% 0 : (at least one copy of rule 42)
%%     (N copies of rule 31)
%%     (N copies of rule 42)
%%
%% Regular expressions are thwarted by having to match an equal amount
%% of rule 31 and rule 42.
%%
%% So, we compute the (finite) regexps for rule 31 and 42, then match
%% *backwards*:
%%
%% 1. As many copies of rule 31 we can find
%% 2. Match the same number of copies of rule 42
%% 3. Remaining string must consist of any number of rule 42, but
%%    nothing else.
%%
%% ======================================================================

part2(Input) ->
  {Rules, Messages} = parse(Input),
  RE_42_str = to_regex(42, Rules),
  {ok, RE_42} = re:compile("^(" ++ RE_42_str ++ ")+$"),
  {ok, RE_42_end} = re:compile("^(?<first>.*)" ++ RE_42_str ++ "$"),
  {ok, RE_31_end} = re:compile("^(?<first>.*)" ++ to_regex(31, Rules) ++ "$"),

  lists:foldl(
    fun(M, Acc) ->
        case is_match(M, RE_42, RE_42_end, RE_31_end) of
          true -> Acc + 1;
          false -> Acc
        end
    end, 0, Messages).

is_match(M, RE_42, RE_42_end, RE_31_end) ->
  {N_31, Rest} = is_match_31(M, RE_31_end, 0),
  if N_31 == 0 -> false;
     true ->
      case is_match_42_n(Rest, RE_42_end, N_31) of
        {true, Rest0} ->
          is_match_42(Rest0, RE_42);
        _ ->
          false
      end
  end.

%% Matches greedily many copies of rule 31 at the end, and returns how
%% many we matched, and the remaining string.
is_match_31(M, RE_31, N) ->
  case re:run(M, RE_31, [{capture, all_names, list}]) of
    {match, [Leading]} ->
      is_match_31(Leading, RE_31, N + 1);
    _ ->
      {N, M}
  end.

%% Matches `N' copies of rule 42 at the end
is_match_42_n(Str, _RE_42_end, 0) ->
  {true, Str};
is_match_42_n(Str, RE_42_end, N) ->
  case re:run(Str, RE_42_end, [{capture, all_names, list}]) of
    {match, [Leading]} ->
      is_match_42_n(Leading, RE_42_end, N - 1);
    _ ->
      false
  end.

%% Matches any copies of rule 42
is_match_42(Str, RE_42) ->
  case re:run(Str, RE_42) of
    {match, _} -> true;
    _ -> false
  end.

%% ======================================================================
%% Graph generator
%% ======================================================================

%% Generate .dot+.png graphs for the rules, to easier spot the
%% differences.
generate_dot_graphs(Rules, Id) ->
  Dot = to_dot(Rules),
  Basename = io_lib:format("day19_~s", [Id]),
  %% ?debugFmt("Generating dot graph: ~s.dot~n", [Basename]),
  ok = file:write_file(Basename ++ ".dot", Dot),
  [] = os:cmd(lists:flatten(io_lib:format("dot -Tpng -o ~s.png ~s.dot",
                                          [Basename, Basename]))).
to_dot(RuleMap) ->
  "digraph rules {\n" ++
    to_dot0(RuleMap) ++
    "}\n".

to_dot0(RuleMap) ->
  lists:map(
    fun({LHS, RHS}) ->
        %% ?debugFmt("~p", [{LHS, RHS}]),
        case RHS of
          [R] ->
            io_lib:format("rule_~w -> rule_~w;~n", [LHS, R]);
          _ ->
            %% If RHS consists of multiple rules, we need an
            %% intermediary node to represent the "logical and".
            RHSNode = make_rhs_node(RHS),
            io_lib:format("rule_~w -> ~s;~n", [LHS, RHSNode]) ++
              lists:map(
                fun(R) ->
                    io_lib:format("~s -> rule_~w;~n",
                                  [RHSNode, R])
                end, RHS)
        end
    end,
    [{LHS, RHS} || {LHS, RHSList} <- maps:to_list(RuleMap),
                   RHS <- RHSList]).

make_rhs_node(RHS) ->
  "_" ++
    lists:join("_and_",
               lists:map(fun(R) when is_atom(R) ->
                             atom_to_list(R);
                            (R) when is_integer(R) ->
                             integer_to_list(R)
                         end, RHS)) ++ "_".


%% ======================================================================
%% Regexp rewriting
%% ======================================================================

to_regex(Key, RuleMap) when is_integer(Key) ->
  case maps:get(Key, RuleMap) of
    [RHS] -> to_regex(RHS, RuleMap);
    [_|_] = RHS ->
      "(" ++
        lists:join(
          "|",
          lists:map(
            fun(X) ->
                to_regex(X, RuleMap)
            end,
            RHS)) ++ ")"
  end;
to_regex(Key, RuleMap) when is_list(Key) ->
  lists:join(
    "",
    lists:map(
      fun(X) ->
          to_regex(X, RuleMap)
      end,
      Key));
to_regex(Key, _RuleMap) when is_atom(Key) ->
  atom_to_list(Key).

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
  [ {"Part 1", ?_assertEqual(147, part1(Input))}
  , {"Part 2", ?_assertEqual(263, part2(Input))}
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
  ?_assertEqual(2, part1(test_input1())).

test_input2() ->
  <<"42: 9 14 | 10 1\n"
    "9: 14 27 | 1 26\n"
    "10: 23 14 | 28 1\n"
    "1: \"a\"\n"
    "11: 42 31 | 42 11 31 \n"
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
    "8: 42 | 42 8\n"
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
  ?_assertEqual(12, part2(test_input2())).

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
