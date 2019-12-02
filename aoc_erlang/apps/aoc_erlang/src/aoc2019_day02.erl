%%% Advent of Code solution for 2019 day 02.
%%% Created: 2019-12-02T08:58:25+00:00

-module(aoc2019_day02).
-include_lib("eunit/include/eunit.hrl").

%% Puzzle solution
part1(Input) ->
  run(Input, 12, 2).

part2(Input) ->
  [100 * A + B || A <- lists:seq(0, 99),
                  B <- lists:seq(0, 99),
                  run(Input, A, B) =:= 19690720].

run(Ints, A, B) ->
  Ints0 = maps:put(1, A, Ints),
  Ints1 = maps:put(2, B, Ints0),
  IntsFinal = do_run(Ints1, 0),
  maps:get(0, IntsFinal).

do_run(Ints, PC) when is_map(Ints) ->
  Get = fun(K) -> maps:get(K, Ints) end,
  Put = fun(K, V) -> maps:put(K, V, Ints) end,
  Op0 = Get(PC),
  case Op0 of
    99 -> Ints;
    _ ->
      Op1 = Get(PC + 1),
      Op2 = Get(PC + 2),
      Op3 = Get(PC + 3),

      case Op0 of
        1 -> do_run(Put(Op3, Get(Op1) + Get(Op2)), PC + 4);
        2 -> do_run(Put(Op3, Get(Op1) * Get(Op2)), PC + 4)
      end
  end.

%% Read input and convert to map from PC (index) -> Value (int).
get_input() ->
  Ints =
    lists:map(fun list_to_integer/1,
              string:tokens(
                inputs:get_as_string(2019, 02), ",")),
  maps:from_list(
    lists:zip(lists:seq(0, length(Ints) - 1),
              Ints)).

%% Tests
main_test_() ->
  Input = get_input(),

  [ {"Part 1", ?_assertEqual(3654868, part1(Input))}
  , {"Part 2", ?_assertEqual([7014], part2(Input))}
  ].

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
