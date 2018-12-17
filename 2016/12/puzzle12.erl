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

testprog() ->
    #{0 => {cpy, 41, a},
      1 => {inc, a},
      2 => {inc, a},
      3 => {dec, a},
      4 => {jnz, a, 2},
      5 => {dec, a}}.

arg(S) ->           
    case re:run(S, "\\d+") of
        {match, _} ->
            list_to_integer(S);
        _ ->
            list_to_atom(S)
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
    %% execute(0, testprog(), #{}).
    execute(0, inputprog(), #{c => 1}).
    %% inputprog().

read_reg(X, Regs) when is_number(X) ->
    X;
read_reg(X, Regs) when is_atom(X) ->
    maps:get(X, Regs, 0).

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
    

    
    
