%%% Advent of Code solution for 2016 day 21.
%%% Created: 2019-11-23T19:15:30+00:00

-module(aoc2016_day21).
-include_lib("eunit/include/eunit.hrl").

%% Puzzle solution
part1(Password, Instrs) ->
  scramble(Password, Instrs).

part2(ScrambledPassword, Instrs) ->
  [U] = lists:filter(fun(P) ->
                         scramble(P, Instrs) =:= ScrambledPassword
                     end, permute("abcdefgh")),
  U.

permute([]) -> [[]];
permute(L) -> [[X|Y] || X <- L, Y <- permute(L -- [X])].

%% Scrambler
scramble(Password, []) ->
  Password;
scramble(Password, [Instr|Instrs]) ->
  I = fun list_to_integer/1,
  P0 =
    case string:tokens(Instr, " ") of
      ["rotate", "based", _, _, _, _, [Letter]] ->
        rotate_pos(Password, Letter);
      ["rotate", "left", Steps, _] ->
        rotate_left(Password, I(Steps));
      ["rotate", "right", Steps, _] ->
        rotate_right(Password, I(Steps));
      ["swap", "position", PosA, _, _, PosB] ->
        swap_pos(Password, I(PosA), I(PosB));
      ["swap", "letter", [LetterA], _, _, [LetterB]] ->
        swap(Password, LetterA, LetterB);
      ["move", "position", PosFrom, _, _, PosTo] ->
        move(Password, I(PosFrom), I(PosTo));
      ["reverse", _, PosFrom, _, PosTo] ->
        reverse(Password, I(PosFrom), I(PosTo))
    end,
  scramble(P0, Instrs).

%% String ops
swap([], _, _) -> [];
swap([A|Rest], A, B) -> [B|swap(Rest, A, B)];
swap([B|Rest], A, B) -> [A|swap(Rest, A, B)];
swap([C|Rest], A, B) -> [C|swap(Rest, A, B)].

swap_pos(S, A, B) ->
  X = lists:nth(A + 1, S),
  Y = lists:nth(B + 1, S),
  swap(S, X, Y).

reverse(S, A, B) ->
  Before = lists:sublist(S, 1, A),
  Reverse = lists:sublist(S, A + 1, B - A + 1),
  After = lists:sublist(S, B + 2, length(S)),
  Before ++ lists:reverse(Reverse) ++ After.

rotate_left(S, A) when A >= length(S) ->
  rotate_left(S, A rem length(S));
rotate_left(S, A) ->
  lists:sublist(S, A + 1, length(S)) ++
    lists:sublist(S, 1, A).

rotate_right(S, A) when A >= length(S) ->
  rotate_right(S, A rem length(S));
rotate_right(S, A) ->
  lists:sublist(S, length(S) - A + 1, length(S)) ++
    lists:sublist(S, 1, length(S) - A).

indexof(S, A) -> indexof(S, A, 0).
indexof([], _, _) -> false;
indexof([A|_], A, N) -> N;
indexof([_|S], A, N) -> indexof(S, A, N + 1).

rotate_pos(S, A) ->
  N = indexof(S, A),
  rotate_right(S, if N >= 4 -> N + 2;
                     true -> N + 1
                  end).

move(S, A, B) ->
  {S1, [X|S2]} = lists:split(A, S),
  S3 = S1 ++ S2,
  {S4, S5} = lists:split(B, S3),
  S4 ++ [X|S5].

%% Input reader (place downloaded input file in
%% priv/inputs/2016/input21.txt).
get_input() ->
  inputs:get_as_lines(2016, 21).

%% Tests
helpers_test_() ->
  {"Helpers",
   [ ?_assertEqual(0, indexof("abcd", $a))
   , ?_assertEqual(3, indexof("abcd", $d))
   , ?_assertEqual(false, indexof("abcd", $z))
   ]}.

string_op_test_() ->
  {"String ops",
   [ ?_assertEqual("acbd", swap("abcd", $b, $c))
   , ?_assertEqual("abedcfgh", reverse("abcdefgh", 2, 4))
   , ?_assertEqual("abgdefch", swap_pos("abcdefgh", 2, 6))
   , ?_assertEqual("cdefghab", rotate_left("abcdefgh", 2))
   , ?_assertEqual("ghabcdef", rotate_right("abcdefgh", 2))
   , ?_assertEqual("cdefghab", rotate_pos("abcdefgh", $e))
   , ?_assertEqual("acdefbgh", move("abcdefgh", 1, 5))
   , ?_assertEqual("aghdfcbe", reverse("abcfdhge", 1, 6))
   ]
  }.

main_test_() ->
  Input = get_input(),
  [ {"Part 1", ?_assertEqual("cbeghdaf", part1("abcdefgh", Input))}
  , {"Part 2", ?_assertEqual("bacdefgh", part2("fbgdceah", Input))}
  ].

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
