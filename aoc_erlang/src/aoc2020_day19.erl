%%% Advent of Code solution for 2020 day 19.
%%% Created: 2020-12-19T06:02:22+00:00

-module(aoc2020_day19).
-include_lib("eunit/include/eunit.hrl").

-compile([nowarn_unused_function, export_all, nowarn_export_all]).

%% Puzzle solution
part1(Input) ->
  [RuleBlock, MessageBlock] = binary:split(Input, <<"\n\n">>),
  Rules = binary:split(RuleBlock, <<"\n">>, [global]),
  Messages = binary:split(MessageBlock, <<"\n">>, [global]),
  Rules0 = lists:foldl(fun parse_rule/2, #{}, Rules),

  lists:foldl(fun(M, Acc) ->
                  RE = to_regex(0, Rules0),
                  case re:run(M, "^" ++ RE ++ "$") of
                    {match, _} -> Acc + 1;
                    _ -> Acc
                  end
              end, 0, Messages).


to_regex(Key, RuleMap) when is_integer(Key) ->
  case maps:get(Key, RuleMap) of
    [RHS] ->
      %% RHS is a single element, no need to parenthesize
      to_regex(RHS, RuleMap);
    [_|_] = RHS ->
      "(" ++ lists:join("|", lists:map(fun(X) -> to_regex(X, RuleMap) end, RHS)) ++ ")"
  end;
to_regex(Key, RuleMap) when is_list(Key) ->
  lists:join("", lists:map(fun(X) -> to_regex(X, RuleMap) end, Key));
to_regex(Key, _RuleMap) when is_atom(Key) ->
  atom_to_list(Key).

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

part2(_Input) ->
  ?debugMsg("Not implemented."),
  0.

%% Input reader (place downloaded input file in
%% priv/inputs/2020/input19.txt).
get_input() ->
  inputs:get_as_binary(2020, 19).

%% Tests
main_test_() ->
  Input = get_input(),
  [ {"Part 1", ?_assertEqual(147, part1(Input))}
  %% , {"Part 2", ?_assertEqual(0, part2(Input))}
  ].

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
  ?_assertEqual(2, part1(test_input())).


%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
