%%% Advent of Code solution for 2020 day 23.
%%% Created: 2020-12-23T05:43:00+00:00

-module(aoc2020_day23).
-include_lib("eunit/include/eunit.hrl").

-define(ONE_MILLION, 1000000).
-define(TEN_MILLION, 10000000).

solve([First|_] = Input, Moves) ->
  init(Input),
  do_moves(First, Moves),
  lists:map(fun(C) -> C + $0 end, tl(to_list(1))).

solve_extended(Input) ->
  solve_extended(Input, ?ONE_MILLION, ?TEN_MILLION).

solve_extended([First|_] = Input, FillTo, Moves) ->
  init(Input),
  fill_to(lists:last(Input), get(max) + 1, FillTo, First),
  put(max, FillTo),
  do_moves(First, Moves),
  A = get(1),
  B = get(A),
  A * B.

fill_to(Prev, Next, FillTo, First) when Next == FillTo ->
  put(Prev, Next),
  put(Next, First);
fill_to(Prev, Next, FillTo, First) ->
  put(Prev, Next),
  fill_to(Next, Next + 1, FillTo, First).

%% @doc Initialize the ring, using the process dictionary (CupNum ->
%% NextCup).
init([First|_] = L) ->
  erase(),
  put(max, lists:max(L)),
  init(L, First).

init([Last], First) ->
  put(Last, First);
init([A,B|L], F) ->
  put(A, B),
  init([B|L], F).

%% Convert the ring to a list, starting at `First'
to_list(First) ->
  to_list(First, get(First), [First]).

to_list(First, Current, Acc) when First == Current ->
  lists:reverse(Acc);
to_list(First, Current, Acc) ->
  to_list(First, get(Current), [Current|Acc]).

do_moves(Current, 0) ->
  Current;
do_moves(Current, N) ->
  A = get(Current),
  B = get(A),
  C = get(B),
  Next = get(C),
  erase(A),
  erase(B),
  erase(C),
  put(Current, Next),
  Dest = find_dest(Current),
  DestNext = get(Dest),
  put(Dest, A),
  put(A, B),
  put(B, C),
  put(C, DestNext),
  do_moves(Next, N - 1).

%% Find the destination cup.
find_dest(Current) ->
  Max = get(max),
  Dest = if Current == 1 -> Max; %% wrap around
            true -> Current - 1
         end,
  case get(Dest) of
    undefined -> find_dest(Dest);
    N when is_integer(N) -> Dest
  end.

%% ======================================================================
%% Helpers
%% ======================================================================

%% Input reader (place downloaded input file in
%% priv/inputs/2020/input23.txt).
get_input() ->
  [5, 8, 6, 4, 3, 9, 1, 7, 2].

%% Tests
main_test_() ->
  Input = get_input(),
  [ {"Part 1", ?_assertEqual("28946753", solve(Input, 100))}
  , {timeout, 600,
     {"Part 2",
      ?_assertEqual(
         519044017360,
         solve_extended(Input))}}
  ].

ex1_test_() ->
  TestInput = [3, 8, 9, 1, 2, 5, 4, 6, 7],
  [ ?_assertEqual("92658374", solve(TestInput, 10))
  , ?_assertEqual("67384529", solve(TestInput, 100))
  ].

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
