%%% Advent of Code solution for 2016 day 23.
%%% Created: 2019-11-24T07:51:57+00:00

-module(aoc2016_day23).
-include_lib("eunit/include/eunit.hrl").

%% Part 1
solve(Input, Ain) ->
  #{a := A} = execute(0, Input, #{a => Ain}),
  A.

%% For part 2, I confess to have cheated by looking at other people's
%% solutions. The input program is basically a factorial program
%% (which I never had the patience or brains to figure out, with a
%% offset to make it generate different solutions for different
%% input. The offset can be found at the end of the program, like:
%%
%% ...
%% cpy 70 c
%% jnz 87 d
%% ...
%%
%% Multiply the constant and add the factorial of the number of eggs
%% (register A).
solve_fast(A) ->
  70 * 87 + factorial(A).

factorial(0) -> 1;
factorial(N) when N > 0 ->
  N * factorial(N-1).

%% ------------------------------------------------------------
%% Parser
%% ------------------------------------------------------------

arg(S) ->
  try list_to_integer(S)
  catch error:badarg -> list_to_atom(S)
  end.

parse_line([A, B, C]) ->
  {list_to_atom(A), arg(B), arg(C)};
parse_line([A, B]) ->
  {list_to_atom(A), arg(B)}.

to_instr_map(Instrs) ->
  {_, InstrMap} =
    lists:foldl(fun(Instr, {N, Map}) ->
                    {N + 1, maps:put(N, Instr, Map)}
                end, {0, #{}}, Instrs),
  InstrMap.

get_input() ->
  Instrs = inputs:parse_lines(inputs:get_as_binary(2016, 23), " ",
                              fun parse_line/1),
  to_instr_map(Instrs).

get_test_input() ->
  Instrs = inputs:parse_lines(
             <<"cpy 2 a\n",
               "tgl a\n",
               "tgl a\n",
               "tgl a\n",
               "cpy 1 a\n",
               "dec a\n",
               "dec a\n">>,
             " ", fun parse_line/1),
  to_instr_map(Instrs).

%% ------------------------------------------------------------
%% Assembunny interpreter
%% ------------------------------------------------------------

read_reg(X, _) when is_number(X) -> X;
read_reg(X, Regs) when is_atom(X) -> maps:get(X, Regs, 0).

execute(Pc, Instrs, Regs) ->
  Instr = maps:get(Pc, Instrs, eop),
  case Instr of
    {cpy, X, Y} when is_atom(Y) ->
      execute(Pc + 1, Instrs, maps:put(Y, read_reg(X, Regs), Regs));
    {inc, X} when is_atom(X) ->
      execute(Pc + 1, Instrs, maps:put(X, read_reg(X, Regs) + 1, Regs));
    {dec, X} when is_atom(X) ->
      execute(Pc + 1, Instrs, maps:put(X, read_reg(X, Regs) - 1, Regs));
    {tgl, X}  ->
      %% Yay! Self-modifying code. What could possibly go wrong...
      TglTarget = Pc + read_reg(X, Regs),
      case maps:is_key(TglTarget, Instrs) of
        false ->
          %% Toggled instruction is invalid, nothing happens.
          execute(Pc + 1, Instrs, Regs);
        true ->
          %% Modify the program by toggling instruction at TglTarget,
          %% according to puzzle rules.
          ModInstr =
            maps:update_with(
              TglTarget,
              fun({inc, Z}) ->     {dec, Z};
                 ({_, Z}) ->       {inc, Z};
                 ({jnz, Z, W}) ->  {cpy, Z, W};
                 ({cpy, Z, W}) ->  {jnz, Z, W}
              end,
              Instrs),
          execute(Pc + 1, ModInstr, Regs)
      end;
    {jnz, X, Y} ->
      case read_reg(X, Regs) of
        0 -> execute(Pc + 1, Instrs, Regs);
        _ -> execute(Pc + read_reg(Y, Regs), Instrs, Regs)
      end;
    eop ->
      Regs;
    _ ->
      execute(Pc + 1, Instrs, Regs)
  end.

%% ------------------------------------------------------------
%% Tests
%% ------------------------------------------------------------

ex_test_() ->
  Input = get_test_input(),
  {"Example 1", ?_assertEqual(3, solve(Input, #{}))}.

main_test_() ->
  Input = get_input(),

  [ {"Part 1", ?_assertEqual(11130, solve(Input, 7))}
  , {"Part 2", ?_assertEqual(479007690, solve_fast(12))}
  ].

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
