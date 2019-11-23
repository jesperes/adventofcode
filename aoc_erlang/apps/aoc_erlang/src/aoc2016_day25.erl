-module(aoc2016_day25).
-include_lib("eunit/include/eunit.hrl").

read_value(Reg, Regs) ->
  case Reg of
    [C] when (C >= $a) and (C =< $z) ->
      maps:get(Reg, Regs, 0);
    _ ->
      list_to_integer(Reg)
  end.

write_reg(Reg, Value, Regs) ->
  Regs#{Reg => Value}.

interpret(_, _, Regs, 0) ->
  Regs;
interpret(PC, Prog, Regs, MaxCycles) ->
  Instr = maps:get(PC, Prog, eop),
  %% erlang:display({{instr, Instr}, {regs, Regs}, {pc, PC}}),
  Next =
    case Instr of
      eop ->
        Regs;
      {cpy, X, Y} ->
        {PC + 1, write_reg(Y, read_value(X, Regs), Regs)};
      {inc, X} ->
        {PC + 1, write_reg(X, read_value(X, Regs) + 1, Regs)};
      {dec, X} ->
        {PC + 1, write_reg(X, read_value(X, Regs) - 1, Regs)};
      {out, X} ->
        %% io:format("Transmitting: ~p~n", [read_value(X, Regs)]),
        {PC + 1, maps:update_with(output, fun(V) ->
                                              [read_value(X, Regs)|V]
                                          end, [], Regs)};
      {jnz, X, Y} ->
        case read_value(X, Regs) of
          0 -> {PC + 1, Regs};
          _ -> {PC + read_value(Y, Regs), Regs}
        end
    end,

  case Next of
    Regs when is_map(Regs) ->
      Regs;
    {PC0, Regs1} ->
      interpret(PC0, Prog, Regs1, MaxCycles - 1)
  end.

find_clock_input() ->
  Prog = parse_prog(),
  find_clock_input(Prog, 0, 100000).

find_clock_input(Prog, A, MaxCycles) ->
  Regs = interpret(0, Prog, #{"a" => A}, MaxCycles),
  case is_alternating(maps:get(output, Regs)) of
    true ->
      {found, A};
    _ ->
      find_clock_input(Prog, A + 1, MaxCycles)
  end.

is_alternating([_]) ->
  true;
is_alternating([X, Y|Rest]) when X /= Y ->
  is_alternating([Y|Rest]);
is_alternating([X, X|_]) ->
  false.

%%% Tests

main_test_() ->
  {"Part 1", ?_assertMatch({found, 180}, find_clock_input())}.

%%% Parser

parse_prog() ->
  Lines = inputs:get_as_lines(2016, 25),
  {_, Prog} = lists:foldl(fun parse_line/2, {0, #{}}, Lines),
  Prog.

parse_line(Line, {PC, Map}) ->
  Tokens = string:tokens(Line, " "),
  Instr =
    case Tokens of
      ["cpy", X, Y] ->
        {cpy, X, Y};
      ["inc", X] ->
        {inc, X};
      ["dec", X] ->
        {dec, X};
      ["jnz", X, Y] ->
        {jnz, X, Y};
      ["out", X] ->
        {out, X}
    end,
  {PC + 1, Map#{PC => Instr}}.
