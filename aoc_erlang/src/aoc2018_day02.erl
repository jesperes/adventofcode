-module(aoc2018_day02).
-include_lib("eunit/include/eunit.hrl").

main_test_() ->
  Lines = inputs:get_as_lines(2018, 2),

  [ {"Part 1", ?_assertEqual(9139, start1(Lines))}
  , {"Part 2", ?_assertEqual("uqcidadzwtnhsljvxyobmkfyr", start2(Lines))}
  ].

%% Build a frequency table of the given string
freq_table([], Table) -> Table;
freq_table([Char|Rest], Table0) ->
  Table = maps:update_with(Char, fun(N) -> N + 1 end, 1, Table0),
  freq_table(Rest, Table).

%% Returns 1 if the Table map contains the value N, 0 otherwise.
has_n(Table, N) ->
  case lists:member(N, maps:values(Table)) of
    true -> 1;
    false -> 0
  end.

start1(Lines) ->
  %% This is a tuple containing the number of strings which has a
  %% letter occurring twice and thrice, respectively.
  {X, Y} =
    lists:foldl(fun(Line, {Twos, Threes}) ->
                    FT = freq_table(Line, #{}),
                    {Twos + has_n(FT, 2),
                     Threes + has_n(FT, 3)}
                end, {0, 0}, Lines),
  X * Y.

number_of_different_chars([], []) ->
  0;
number_of_different_chars([X|Xs], [X|Ys]) ->
  0 + number_of_different_chars(Xs, Ys);
number_of_different_chars([_|Xs], [_|Ys]) ->
  1 + number_of_different_chars(Xs, Ys).

remove_diff_char([], []) ->
  [];
remove_diff_char([X|Xs], [X|Ys]) ->
  [X|remove_diff_char(Xs, Ys)];
remove_diff_char([_|Xs], [_|Ys]) ->
  remove_diff_char(Xs, Ys).

start2(Lines) ->
  DiffList = [{X, Y, number_of_different_chars(X, Y)} ||
               X <- Lines, Y <- Lines, X < Y],

  [Solution] =
    lists:filtermap(
      fun({X, Y, N}) ->
          if N =:= 1 ->
              {true, remove_diff_char(X, Y)};
             true ->
              false
          end
      end, DiffList),
  Solution.
