%%% Advent of Code solution for 2020 day 19.
%%% Created: 2020-12-19T06:02:22+00:00

-module(aoc2020_day19).
-include_lib("eunit/include/eunit.hrl").

%% Puzzle solution
part1(Input) ->
  [RuleBlock, MessageBlock] = binary:split(Input, <<"\n\n">>),
  Rules = lists:sort(binary:split(RuleBlock, <<"\n">>, [global])),
  Messages = binary:split(MessageBlock, <<"\n">>, [global]),
  ParsedRules = lists:map(fun parse_rule/1, Rules),
  {ParsedRules, Messages}.

parse_rule(Binary) ->
  [LHS, RHS] = binary:split(Binary, <<":">>),
  SubRules = lists:map(
               fun(B) ->
                   lists:filtermap(
                     fun(B0) ->
                         case byte_size(B0) > 0 of
                           true -> {true, B0};
                           false -> false
                         end
                     end,
                     binary:split(B, <<" ">>, [global]))
               end, binary:split(RHS, <<"|">>, [global])),
  {LHS, SubRules}.


reduce(Message) ->






%%   FileName = code:lib_dir(aoc_erlang, src) ++ "/" ++ atom_to_list(?MODULE) ++ "_parser.yrl",
%%   emit_yrl(ParsedRules, FileName),
%%   try
%%     %% There will be warnings
%%     {ok, _, _Warnings} = yecc:file(FileName, [return])
%%   after
%%       file:delete(FileName)
%%   end.

%% emit_yrl(Rules, F) ->
%%   Yrl =
%%     io_lib:format("Nonterminals\n"
%%                   "    ~s.\n"
%%                   "\n"
%%                   "Terminals\n"
%%                   "    'a' 'b'.\n"
%%                   "\n"
%%                   "Rootsymbol\n"
%%                   "    rule0.\n"
%%                   "\n"
%%                   "~s~n",
%%                   [ lists:join(" ", lists:map(fun emit_non_terminal/1, Rules))
%%                   , lists:map(fun emit_rule/1, Rules)
%%                   ]),
%%   io:format("~n~s~n", [Yrl]),
%%   ok = file:write_file(F, Yrl).

%% emit_non_terminal({LHS, _}) ->
%%   io_lib:format("rule~s", [LHS]).

%% emit_rule({LHS, RHS}) ->
%%   lists:join("",
%%              lists:map(fun(Rule) ->
%%                            emit_rule0(LHS, Rule)
%%                        end, RHS)).

%% emit_rule0(LHS, RHS) ->
%%   io_lib:format("rule~s -> ~s : { '$1' }.~n",
%%                 [LHS,
%%                  lists:join(
%%                    " ",
%%                    lists:map(
%%                      fun(R) ->
%%                          case R of
%%                            <<"\"a\"">> -> io_lib:format("'a'", []);
%%                            <<"\"b\"">> -> io_lib:format("'b'", []);
%%                            _ -> io_lib:format("rule~s", [R])
%%                          end
%%                      end, RHS))]).

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

  [ {"Part 1", ?_assertEqual(0, part1(Input))}
  , {"Part 2", ?_assertEqual(0, part2(Input))}
  ].

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
