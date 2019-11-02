%%% @author Jesper Eskilson <jesper.eskilson@klarna.com>
%%% @copyright (C) 2019, Jesper Eskilson
%%% @doc
%%%
%%% @end
%%% Created :  1 Mar 2019 by Jesper Eskilson <jesper.eskilson@klarna.com>

-module(aoc2015_day01).
-behavior(aoc_solution).

-export([start/0]).

start() ->
  Bin = aoc_inputs:get_as_binary(2015, 1),
  count_floors(Bin, 0, 0, undef).

count_floors(<<>>, Acc, _, Neg) -> {Acc, Neg};
count_floors(Bin, Acc, Pos, undef) when Acc < 0 ->
  count_floors(Bin, Acc, Pos, Pos);
count_floors(<<$(,Rest/binary>>, Acc, Pos, Neg) ->
  count_floors(Rest, Acc + 1, Pos + 1, Neg);
count_floors(<<$),Rest/binary>>, Acc, Pos, Neg) ->
  count_floors(Rest, Acc - 1, Pos + 1, Neg).
