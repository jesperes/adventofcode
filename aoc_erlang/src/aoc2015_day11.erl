-module(aoc2015_day11).

-include_lib("eunit/include/eunit.hrl").

main_test_() ->
  Input = "cqjxjnds",
  P1 = "cqjxxyzz",
  P2 = "cqkaabcc",

  [ {"Part 1", fun() -> ?assertEqual(P1, next_valid_password(Input)) end}
  , {"Part 2", fun() -> ?assertEqual(P2, next_valid_password(P1)) end}
  ].

rev(S) ->
  lists:reverse(S).

increment(Pwd) ->
  rev(increment0(rev(Pwd))).

increment0([]) ->
  [];
increment0([$z|Xs]) ->
  [$a|increment0(Xs)];
increment0([C|Xs]) ->
  [C + 1|Xs].

has_straight([]) ->
  false;
has_straight([X,Y,Z|_]) when (Y == X + 1) and (Z == Y + 1) ->
  true;
has_straight([_|Rest]) ->
  has_straight(Rest).

no_confusing_letters(Pwd) ->
  not (lists:member($i, Pwd) or
       lists:member($o, Pwd) or
       lists:member($l, Pwd)).

%% Password are always 8 chars, so there are only a handful of
%% combinations of having two distinct pairs.
two_pairs([A, A, B, B, _, _, _, _]) when A =/= B -> true;
two_pairs([A, A, _, B, B, _, _, _]) when A =/= B -> true;
two_pairs([A, A, _, _, B, B, _, _]) when A =/= B -> true;
two_pairs([A, A, _, _, _, B, B, _]) when A =/= B -> true;
two_pairs([A, A, _, _, _, _, B, B]) when A =/= B -> true;
two_pairs([_, A, A, B, B, _, _, _]) when A =/= B -> true;
two_pairs([_, A, A, _, B, B, _, _]) when A =/= B -> true;
two_pairs([_, A, A, _, _, B, B, _]) when A =/= B -> true;
two_pairs([_, A, A, _, _, _, B, B]) when A =/= B -> true;
two_pairs([_, _, A, A, B, B, _, _]) when A =/= B -> true;
two_pairs([_, _, A, A, _, B, B, _]) when A =/= B -> true;
two_pairs([_, _, A, A, _, _, B, B]) when A =/= B -> true;
two_pairs([_, _, _, A, A, B, B, _]) when A =/= B -> true;
two_pairs([_, _, _, A, A, _, B, B]) when A =/= B -> true;
two_pairs([_, _, _, _, A, A, B, B]) when A =/= B -> true;
two_pairs(_) -> false.

valid(Pwd) ->

  %% * Passwords must include one increasing straight of at least
  %%   three letters, like abc, bcd, cde, and so on, up to xyz. They
  %%   cannot skip letters; abd doesn't count.
  %%
  %% * Passwords may not contain the letters i, o, or l, as these
  %%   letters can be mistaken for other characters and are
  %%   therefore confusing.
  %%
  %% * Passwords must contain at least two different,
  %%   non-overlapping pairs of letters, like aa, bb, or zz.

  has_straight(Pwd) and
    no_confusing_letters(Pwd) and
    two_pairs(Pwd).

next_valid_password(Pwd) ->
  Next = increment(Pwd),
  case valid(Next) of
    true -> Next;
    false -> next_valid_password(increment(Next))
  end.
