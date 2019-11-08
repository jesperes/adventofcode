-module(aoc2015_day01).

-include_lib("eunit/include/eunit.hrl").

-spec count_floors(Input :: binary(),
                   Acc :: integer(),
                   Pos :: integer(),
                   Neg :: integer() | 'undef') ->
                      { integer(), integer() }.
count_floors(<<>>, Acc, _, Neg) -> {Acc, Neg};
count_floors(Bin, Acc, Pos, undef) when Acc < 0 ->
  count_floors(Bin, Acc, Pos, Pos);
count_floors(<<$(,Rest/binary>>, Acc, Pos, Neg) ->
  count_floors(Rest, Acc + 1, Pos + 1, Neg);
count_floors(<<$),Rest/binary>>, Acc, Pos, Neg) ->
  count_floors(Rest, Acc - 1, Pos + 1, Neg).

main_test() ->
  Bin = inputs:get_as_binary(2015, 1),
  {232, 1783} = count_floors(Bin, 0, 0, undef).
