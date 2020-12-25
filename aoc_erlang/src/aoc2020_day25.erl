%%% Advent of Code solution for 2020 day 25.
%%% Created: 2020-12-25T06:04:42+00:00

-module(aoc2020_day25).
-include_lib("eunit/include/eunit.hrl").

-define(NUM_20201227, 20201227).

-compile([nowarn_unused_function]).

part1({CardPK, DoorPK}) ->
  %% The problem is symmetrical. We happen to know that the door's
  %% loop size is smaller (~1M vs ~10M), so that will be faster both
  %% in terms of finding the loop size and calculating the key.
  DLS = find_loop_size(7, DoorPK),
  powmod(CardPK, DLS, ?NUM_20201227).

find_loop_size(Subject, PK) ->
  find_loop_size(1, Subject, Subject, PK).

find_loop_size(N, Subject, Current, PK) ->
  PK0 = (Current * Subject) rem ?NUM_20201227,
  if PK0 == PK -> N + 1;
     true -> find_loop_size(N + 1, Subject, PK0, PK)
  end.

powmod(A, B, M) -> powmod(A, B, M, 1).
powmod(_, 0, _, R) -> R;
powmod(A, B, M, R) when B rem 2 == 1 -> powmod(A, B-1, M, A*R rem M);
powmod(A, B, M, R) -> powmod(A*A rem M, B div 2, M, R).

%% ======================================================================
%% Tests

get_input() ->
  { 15113849 %% Card public key
  , 4206373  %% Door public key
  }.

test_input() ->
  { 5764801   %% Card public key
  , 17807724  %% Door public key
  }.

%% Tests
main_test_() ->
  {"Part 1", ?_assertEqual(1890859, part1(get_input()))}.

ex1_test_() ->
  ?_assertEqual(14897079, part1(test_input())).

find_loop_size_test_() ->
  {CardPK, DoorPK} = test_input(),
  [ ?_assertEqual(8, find_loop_size(7, CardPK))
  , ?_assertEqual(11, find_loop_size(7, DoorPK))
  ].

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
