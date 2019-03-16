%%%-------------------------------------------------------------------
%%% @author Jesper Eskilson <>
%%% @copyright (C) 2018, Jesper Eskilson
%%% @doc
%%%
%%% @end
%%% Created : 17 Dec 2018 by Jesper Eskilson <>
%%%-------------------------------------------------------------------
-module(puzzle12).
-export([start/0]).

arg(S) ->   
    try list_to_integer(S)
    catch error:badarg -> list_to_atom(S)
    end.

inputprog() ->
    {ok, Binary} = file:read_file("input.txt"),
    Instrs = 
        lists:map(fun(Line) ->
                          case string:tokens(Line, " ") of
                              [A, B, C] ->
                                  {list_to_atom(A), arg(B), arg(C)};
                              [A, B] ->
                                  {list_to_atom(A), arg(B)}
                          end
                  end,
                  string:tokens(binary_to_list(Binary), "\n")),
    
    {_, InstrMap} = 
        lists:foldl(fun(Instr, {N, Map}) ->
                            {N + 1, maps:put(N, Instr, Map)}
                    end, {0, #{}}, Instrs),
    InstrMap.

start() ->
    Prog = inputprog(),
    #{a := A1} = execute(0, Prog, #{}),
    #{a := A2} = execute(0, Prog, #{c => 1}),
    {A1, A2}.

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
    

    
    
