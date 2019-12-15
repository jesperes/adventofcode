%%% Advent of Code solution for 2016 day 19.
%%% Created: 2019-12-15T17:50:57+00:00

-module(aoc2016_day19).
-include_lib("eunit/include/eunit.hrl").

get_input() -> 3014387.

%% Day 19: An Elephant Named Joseph
%%
%% This is a variant of the Josephus problem, which can be solved
%% arithmetically, see https://www.youtube.com/watch?v=uCsD3ZGzMgE.
part1(N) ->
  2*(N - largest_2pow(1, N)) + 1.

%% Return the largest 2^A such that 2^A < N. I'm sure there is a nicer
%% way of doing this.
largest_2pow(A, N) ->
  if (1 bsl A) > N -> 1 bsl (A - 1);
     true -> largest_2pow(A + 1, N)
  end.

%% Return the largest 3^A < N.
largest_3pow(A, N) ->
  P = math:pow(3, A),
  if P > N -> floor(math:pow(3, A - 1));
     true  -> largest_3pow(A + 1, N)
  end.

%% Part 2 can also be solved arithmetically, but I stole the formula
%% from Reddit:
%% https://www.reddit.com/r/adventofcode/comments/5j4lp1/2016_day_19_solutions/
part2(N) ->
  B = largest_3pow(1, N),
  if N == B     -> N;
     N - B =< B -> N - B;
     true       -> 2 * N - 3 * B
  end.

%% Tests
main_test_() ->
  Input = get_input(),

  [ {"Part 1", ?_assertEqual(1834471, part1(Input))}
  , {"Part 2", ?_assertEqual(1420064, part2(Input))}
  ].

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
