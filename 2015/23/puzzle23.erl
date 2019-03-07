-module(puzzle23).
-export([start/0]).
-include_lib("eunit/include/eunit.hrl").

start() ->
    Prog = parse(real_data()),
    {execute_program(Prog, #{pc => 0, a => 0, b => 0}),
     execute_program(Prog, #{pc => 0, a => 1, b => 0})}.

execute_program(Prog, #{pc := PC} = Regs) ->
    case maps:get(PC, Prog, eop) of
        eop -> 
            maps:get(b, Regs);
        Instr -> 
            Regs0 = execute_instr(Instr, Regs),
            execute_program(Prog, Regs0)
    end.

%%% Instruction execution

execute_instr({inc, Reg}, Regs) ->
    incr_pc(maps:update_with(Reg, fun(V) -> V + 1 end, Regs));
execute_instr({hlf, Reg}, Regs) ->
    incr_pc(maps:update_with(Reg, fun(V) -> V div 2 end, Regs));
execute_instr({tpl, Reg}, Regs) ->
    incr_pc(maps:update_with(Reg, fun(V) -> V * 3 end, Regs));
execute_instr({jmp, Offset}, Regs) ->
    maps:update_with(pc, fun(V) -> V + Offset end, Regs);
execute_instr({jie, Reg, Offset}, Regs) ->
    case is_even(Reg, Regs) of
        true -> maps:update_with(pc, fun(V) -> V + Offset end, Regs);
        false -> incr_pc(Regs)
    end;
execute_instr({jio, Reg, Offset}, Regs) ->
    case is_one(Reg, Regs) of
        true -> maps:update_with(pc, fun(V) -> V + Offset end, Regs);
        false -> incr_pc(Regs)
    end.

%%% Parser

parse(Binary) ->
    P = lists:foldl(fun parse_line/2, #{pc => 0},
                    string:tokens(binary_to_list(Binary), "\n")),
    maps:remove(pc, P). %% pc is only used temporarily during parsing

parse_line(Line, #{pc := PC} = Prog) ->
    Prog#{PC => parse_tokens(string:tokens(Line, ", ")), pc => PC + 1}.

parse_tokens(["hlf", Reg]) -> {hlf, ltoa(Reg)};
parse_tokens(["tpl", Reg]) -> {tpl, ltoa(Reg)};
parse_tokens(["inc", Reg]) -> {inc, ltoa(Reg)};
parse_tokens(["jmp", Offset]) -> {jmp, ltoi(Offset)};
parse_tokens(["jie", Reg, Offset]) -> {jie, ltoa(Reg), ltoi(Offset)};
parse_tokens(["jio", Reg, Offset]) -> {jio, ltoa(Reg), ltoi(Offset)}.

%%% Helpers

ltoa(X) -> list_to_atom(X).
ltoi(X) -> list_to_integer(X).

incr_pc(Regs) -> maps:update_with(pc, fun(V) -> V + 1 end, Regs).

is_one(Reg, Regs) -> maps:get(Reg, Regs) == 1.
is_even(Reg, Regs) -> maps:get(Reg, Regs) rem 2 == 0.

%%% Tests

real_data() ->
    {ok, Binary} = file:read_file("input.txt"),
    Binary.

test_data() ->
    <<"inc a\n",
      "jio a, +2\n",
      "tpl a\n",
      "inc a\n">>.

part1_ex1_test() ->
    Prog = parse(test_data()),
    ?assertMatch(#{a := 2}, execute_program(Prog, #{pc => 0, a => 0, b => 0})).

part1_real_test() ->
    Prog = parse(real_data()),
    ?assertMatch(#{b := 255}, execute_program(Prog, #{pc => 0, a => 0, b => 0})).

part2_real_test() ->
    Prog = parse(real_data()),
    ?assertMatch(#{b := 334}, execute_program(Prog, #{pc => 0, a => 1, b => 0})).
