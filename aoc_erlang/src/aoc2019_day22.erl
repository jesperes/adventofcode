%%% Advent of Code solution for 2019 day 22.
%%% Created: 2019-12-22T07:08:23+00:00

-module(aoc2019_day22).
-include_lib("eunit/include/eunit.hrl").

part1(Lines) ->
  Instrs = parse(Lines),
  Deck = shuffle(Instrs, lists:seq(0, 10006)),
  find_card(2019, Deck, 0).

find_card(_, [], _) -> false;
find_card(Card, [Card|_], N) -> N;
find_card(Card, [_|Deck], N) ->
  find_card(Card, Deck, N + 1).

shuffle([], Deck) -> Deck;
shuffle([Instr|Instrs], Deck) ->
  case Instr of
    {cut, Cut} when Cut >= 0 ->
      {L1, L2} = lists:split(Cut, Deck),
      shuffle(Instrs, L2 ++ L1);
    {cut, Cut} when Cut < 0->
      {L1, L2} = lists:split(length(Deck) + Cut, Deck),
      shuffle(Instrs, L2 ++ L1);
    {incr, Incr} ->
      Len = length(Deck),
      {Positions, _} = lists:split(Len, lists:seq(0, Len * Incr, Incr)),
      Tree =
        lists:foldl(fun({Pos, Card}, Acc) ->
                        Index = Pos rem Len,
                        gb_trees:insert(Index, Card, Acc)
                    end, gb_trees:empty(),
                    lists:zip(Positions, Deck)),
      Deck0 = gb_trees:values(Tree),
      shuffle(Instrs, Deck0);
    new ->
      shuffle(Instrs, lists:reverse(Deck))
  end.

%% For part 2 we need some number theory which I was not able to
%% figure out myself. The code below is assembled from Python snippets
%% mostly from the day 22 reddit solution megathread.
part2(Lines) ->
  N = 101741582076661, %% Reps
  D = 119315717514047, %% Deck size
  X = 2020,            %% The position we are interested in

  %% Run the shuffling backwards twice to get Y and Z.
  Instrs = parse(Lines),
  RInstrs = lists:reverse(Instrs),
  Y = shuffle2(X, RInstrs, D),
  Z = shuffle2(Y, RInstrs, D),

  %% Apply number theory to compute what card eventually ends up in
  %% position X.
  A = mod((Y - Z) * modinv(X - Y + D, D), D),
  B = mod(Y - A*X, D),
  (powmod(A, N, D) * X +
     (powmod(A, N, D) - 1) * modinv(A - 1, D) * B) rem D.

%% Apply the shuffling rules to the card at position X.
shuffle2(X, [], _) -> X;
shuffle2(X, [R|RInstrs], D) ->
  case R of
    {cut, Cut} -> shuffle2((X + Cut + D) rem D, RInstrs, D);
    {incr, Incr} -> shuffle2(modinv(Incr, D) * X, RInstrs, D);
    new -> shuffle2(D - 1 - X, RInstrs, D)
  end.

%% Remainder which handles negative numbers "correctly".
mod(X, Y) -> mod0(X rem Y, Y).
mod0(M, _) when M >= 0 -> M;
mod0(M, Y) -> mod0(M + Y, Y).

%% Modular multiplicative inverse from
%% https://stackoverflow.com/a/9758173/13051
egcd(0, B) -> {B, 0, 1};
egcd(A, B) ->
  {G, Y, X} = egcd(mod(B, A), A),
  {G, X - (B div A) * Y, Y}.

modinv(A, M) ->
  {G, X, _} = egcd(A, M),
  case G of
    -1 -> throw(mod_inv_does_not_exist);
    _ -> mod(X, M)
  end.

%% Fast modular exponentiation from
%% https://gist.github.com/Radcliffe/e41b41a441deda19e7ac5731197f49be
powmod(A, B, M) -> powmod(A, B, M, 1).
powmod(_, 0, _, R) -> R;
powmod(A, B, M, R) when B rem 2 == 1 -> powmod(A, B-1, M, A*R rem M);
powmod(A, B, M, R) -> powmod(A*A rem M, B div 2, M, R).

parse(Lines) ->
  ToI = fun list_to_integer/1,
  lists:map(fun(Line) ->
                case string:tokens(Line, " ") of
                  ["cut", Cut] ->
                    {cut, ToI(Cut)};
                  ["deal", "with", "increment", Incr] ->
                    {incr, ToI(Incr)};
                  ["deal", "into", "new", "stack"] ->
                    new
                end
            end, Lines).

%% Input reader (place downloaded input file in
%% priv/inputs/2019/input22.txt).
get_input() ->
  inputs:get_as_lines(2019, 22).

%% Tests
main_test_() ->
  Lines = get_input(),

  [ {"Part 1", ?_assertEqual(4775, part1(Lines))}
  , {"Part 2", ?_assertEqual(37889219674304, part2(Lines))}
  ].

ex1_test_() ->
  Lines = [ "cut 6"
          , "deal with increment 7"
          , "deal into new stack"
          ],
  Instrs = parse(Lines),
  Result = [3, 0, 7, 4, 1, 8, 5, 2, 9, 6],
  ?_assertEqual(Result, shuffle(Instrs, lists:seq(0, 9))).

ex2_test_() ->
  Lines = [ "deal into new stack"
          , "cut -2"
          , "deal with increment 7"
          , "cut 8"
          , "cut -4"
          , "deal with increment 7"
          , "cut 3"
          , "deal with increment 9"
          , "deal with increment 3"
          , "cut -1"
          ],
  Instrs = parse(Lines),
  Result = [9, 2, 5, 8, 1, 4, 7, 0, 3, 6],
  ?_assertEqual(Result, shuffle(Instrs, lists:seq(0, 9))).

modinv_test_() ->
  ?_assertEqual(10, modinv(573, 17)).

mod_test() ->
  ?_assertEqual(2, mod(-10, 3)).

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
