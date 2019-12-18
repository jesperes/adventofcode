%%% Advent of Code solution for 2019 day 17.
%%% Created: 2019-12-17T16:31:23+00:00

-module(aoc2019_day17).
-include_lib("eunit/include/eunit.hrl").

part1(Prog) ->
  {_, Scaffolds} = parse(Prog),
  count_intersections(Scaffolds).

part2(Prog) ->
  {_, Scaffolds} = parse(Prog),

  %% Split the necessary instructions into functions < 20 chars/line.
  A = "L,12,L,12,L,6,L,6",
  B = "R,8,R,4,L,12",
  C = "L,12,L,6,R,12,R,8",
  M = "A,B,A,C,B,A,C,B,A,C",

  Input = io_lib:format("~s~n~s~n~s~n~s~nn~n", [M, A, B, C]),
  Prog0 = maps:merge(Prog, #{0 => 2}),
  %% Score is the last integer output by the program, but this
  %% implementation returns the output in reverse order.
  {_, [Score|Output]} = intcode:execute(Prog0, Input),
  Score.

parse(Prog) ->
  {_, List} = intcode:execute(Prog),
  L = lists:reverse(List),
  Bin = list_to_binary(L),
  [{Width, _}|_] = binary:matches(Bin, <<"\n">>),
  Scaffolds = make_grid(Bin, Width + 1),
  {L, Scaffolds}.

count_intersections(Scaffolds) ->
  maps:fold(
    fun({X, Y}, _, Acc) ->
        N = {X, Y - 1},
        E = {X + 1, Y},
        S = {X, Y + 1},
        W = {X - 1, Y},
        case maps:is_key(N, Scaffolds) andalso
          maps:is_key(S, Scaffolds) andalso
          maps:is_key(E, Scaffolds) andalso
          maps:is_key(W, Scaffolds) of
          true ->
            Acc + (X * Y);
          false ->
            Acc
        end
    end, 0, Scaffolds).

make_grid(Bin, Width) ->
  ScaffoldingChars = [<<"#">>, <<"<">>, <<">">>, <<"v">>, <<"^">>],
  maps:from_list(
    [{{Start rem Width, Start div Width}, $*}
     || {Start, _} <- binary:matches(Bin, ScaffoldingChars)]).


%% Input reader (place downloaded input file in
%% priv/inputs/2019/input17.txt).
get_input() ->
  inputs:get_as_binary(2019, 17).

%% Tests
main_test_() ->
  Input = intcode:parse(get_input()),

  [ {"Part 1", ?_assertEqual(4800, part1(Input))}
  , {"Part 2", ?_assertEqual(982279, part2(Input))}
  ].

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
