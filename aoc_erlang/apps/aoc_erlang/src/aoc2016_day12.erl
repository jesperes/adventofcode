-module(aoc2016_day12).

-include_lib("eunit/include/eunit.hrl").

arg(S) ->
  try list_to_integer(S)
  catch error:badarg -> list_to_atom(S)
  end.

inputprog() ->
  Lines = inputs:get_as_lines(2016, 12),
  Instrs =
    lists:map(fun(Line) ->
                  case string:tokens(Line, " ") of
                    [A, B, C] ->
                      {list_to_atom(A), arg(B), arg(C)};
                    [A, B] ->
                      {list_to_atom(A), arg(B)}
                  end
              end, Lines),

  {_, InstrMap} =
    lists:foldl(fun(Instr, {N, Map}) ->
                    {N + 1, maps:put(N, Instr, Map)}
                end, {0, #{}}, Instrs),
  InstrMap.

main_test_() ->
  Prog = inputprog(),
  P1 = 318003,
  P2 = 9227657,

  [ {"Part 1", ?_assertMatch(#{a := P1}, execute(0, Prog, #{}))}
  , {"Part 2", ?_assertMatch(#{a := P2}, execute(0, Prog, #{c => 1}))}
  ].

read_reg(X, _) when is_number(X) -> X;
read_reg(X, Regs) when is_atom(X) -> maps:get(X, Regs, 0).

execute(Pc, Instrs, Regs) ->
  Instr = maps:get(Pc, Instrs, eop),
  case Instr of
    {cpy, X, Y} ->
      execute(Pc + 1, Instrs, maps:put(Y, read_reg(X, Regs), Regs));
    {inc, X} ->
      execute(Pc + 1, Instrs, maps:put(X, read_reg(X, Regs) + 1, Regs));
    {dec, X} ->
      execute(Pc + 1, Instrs, maps:put(X, read_reg(X, Regs) - 1, Regs));
    {jnz, X, Y} ->
      case read_reg(X, Regs) of
        0 ->
          execute(Pc + 1, Instrs, Regs);
        _ ->
          execute(Pc + Y, Instrs, Regs)
      end;
    eop ->
      Regs
  end.
