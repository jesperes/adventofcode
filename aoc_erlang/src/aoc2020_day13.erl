%%% Advent of Code solution for 2020 day 13.
%%% Created: 2020-12-13T06:55:07+00:00

-module(aoc2020_day13).
-include_lib("eunit/include/eunit.hrl").

%% Puzzle solution
part1(Lines) ->
  [TimestampStr, DepartureStr] = Lines,
  TS = list_to_integer(TimestampStr),
  Deps = lists:map(fun list_to_integer/1,
                   re:split(DepartureStr, "[,x]+", [{return, list}])),
  find_next_dep(TS, Deps).

find_next_dep(TS, Deps) ->
  [{T, ID}|_] =
    lists:sort(lists:map(fun(ID0) ->
                             {ID0 - (TS rem ID0), ID0}
                         end, Deps)),
  T * ID.

part2(Lines) ->
  [_, DepartureStr] = Lines,
  L = re:split(DepartureStr, ",", [{return, list}]),
  Congruences =
    lists:filtermap(fun({_, "x"}) -> false;
                       ({A, N}) ->
                        Modulo = list_to_integer(N), % aka bus ID
                        Residue = Modulo - A,
                        {true, {Residue, Modulo}}
                    end,
                    lists:zip(lists:seq(0, length(L)-1), L)),
  chinese_remainder(Congruences).

%% ======================================================================
%% CRT impl from https://rosettacode.org/wiki/Chinese_remainder_theorem#Erlang
%% ======================================================================

egcd(_, 0) -> {1, 0};
egcd(A, B) ->
  {S, T} = egcd(B, A rem B),
  {T, S - (A div B)*T}.

mod(A, M) ->
  X = A rem M,
  if X < 0 -> X + M;
     true -> X
  end.

mod_inv(A, B) ->
  {X, Y} = egcd(A, B),
  if
    A*X + B*Y =:= 1 -> X;
    true -> undefined
  end.

calc_inverses([], []) -> [];
calc_inverses([N | Ns], [M | Ms]) ->
  case mod_inv(N, M) of
    undefined -> undefined;
    Inv -> [Inv | calc_inverses(Ns, Ms)]
  end.

chinese_remainder(Congruences) ->
  {Residues, Modulii} = lists:unzip(Congruences),
  ModPI = lists:foldl(fun(A, B) -> A*B end, 1, Modulii),
  CRT_Modulii = [ModPI div M || M <- Modulii],
  case calc_inverses(CRT_Modulii, Modulii) of
    undefined -> undefined;
    Inverses ->
      Solution =
        lists:sum([A*B ||
                    {A,B} <-
                      lists:zip(
                        CRT_Modulii,
                        [A*B || {A,B} <- lists:zip(
                                           Residues,
                                           Inverses)
                        ])
                  ]),
      mod(Solution, ModPI)
  end.

%% Input reader (place downloaded input file in
%% priv/inputs/2020/input13.txt).
get_input() ->
  inputs:get_as_lines(2020, 13).

%% Tests
main_test_() ->
  Input = get_input(),

  [ {"Part 1", ?_assertEqual(333, part1(Input))}
  , {"Part 2", ?_assertEqual(690123192779524, part2(Input))}
  ].

ex1_test_() ->
  ?_assertEqual(295, find_next_dep(939, [7, 13, 59, 31, 19])).

ex2_test_() ->
  [ ?_assertEqual(1068781, part2(["", "7,13,x,x,59,x,31,19"]))
  , ?_assertEqual(3417, part2(["", "17,x,13,19"]))
  , ?_assertEqual(754018, part2(["", "67,7,59,61"]))
  ].

%% Test cases from https://rosettacode.org/wiki/Chinese_remainder_theorem#Erlang.
crt_test_() ->
  [ ?_assertEqual(1000, chinese_remainder([{10 , 11},
                                           {4  , 12},
                                           {12 , 13}]))
  , ?_assertEqual(undefined, chinese_remainder([{10 , 11},
                                                {4  , 22},
                                                {9  , 19}]))
  , ?_assertEqual(23, chinese_remainder([{2 , 3},
                                         {3 , 5},
                                         {2 , 7}]))
  ].

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
