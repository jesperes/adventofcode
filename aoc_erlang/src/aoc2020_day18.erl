%%% Advent of Code solution for 2020 day 18.
%%% Created: 2020-12-18T06:16:04+00:00

-module(aoc2020_day18).
-include_lib("eunit/include/eunit.hrl").

-define(is_int_char(C), (C >= $0) and (C =< $9)).
-define(is_space(C), (C == 32)).

%% Solve both parts at once, but use different parsers. They only
%% differ in the precedence of the +/* operators.
parts(Input) ->
  lists:foldl(fun(Line, {A1, A2}) ->
                  {ok, Tokens, _} = aoc2020_day18_lexer:string(Line),
                  {A1 + eval_tokens(Tokens, aoc2020_day18_part1_parser),
                   A2 + eval_tokens(Tokens, aoc2020_day18_part2_parser)}
              end, {0, 0}, Input).

eval_tokens(Tokens, Parser) ->
  {ok, AST} = Parser:parse(Tokens),
  eval_ast(AST).

eval_ast({int, N}) -> N;
eval_ast({expr, Expr}) -> eval_ast(Expr);
eval_ast({plus, A, B}) -> eval_ast(A) + eval_ast(B);
eval_ast({mult, A, B}) -> eval_ast(A) * eval_ast(B).

%% Input reader (place downloaded input file in
%% priv/inputs/2020/input18.txt).
get_input() ->
  inputs:get_as_lines(2020, 18).

%% Tests
main_test_() ->
  Input = get_input(),
  {"Part 1 & 2", ?_assertEqual({8298263963837, 145575710203332}, parts(Input))}.

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
