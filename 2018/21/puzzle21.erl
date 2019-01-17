-module(puzzle21).

-export([main/0]).
-compile([export_all]).

main() ->
    {{part1, 0},
     {part2, 0}}.

start(R0) ->
    {IpReg, Prog} = read_program("input.txt"),
    emulate(Prog, IpReg, [R0, 0, 0, 0, 0, 0]).

read_program(Filename) ->
    {ok, Binary} = file:read_file(Filename),
    ProgAsStr = binary_to_list(Binary),
    [IpLine|Lines] = string:tokens(ProgAsStr, "\n"),
    ["#ip", IpStr] = string:tokens(IpLine, " "),
    Ip = list_to_integer(IpStr),
    {_, Map} = 
        lists:foldl(fun(X, {N, Map}) ->
                            Tokens = string:tokens(X, " \t"),
                            {N + 1, maps:put(N, parse_line(Tokens), Map)}
                    end, {0, #{}}, Lines),
    {Ip, Map}.

parse_line([Op, A, B, C, "#"|_]) ->
    parse_line([Op, A, B, C]);
parse_line([Op, A, B, C]) ->
    {list_to_atom(Op), 
     list_to_integer(A),
     list_to_integer(B),
     list_to_integer(C)}.

trace({eqrr, 1, 0, 5}, Regs, _, _Cycle, Acc) ->
    R1 = read_reg(1, Regs),
    %% erlang:display({num_r1_seen, maps:size(Acc)}),
    case maps:is_key(R1, Acc) of
        true ->
            {part2, maps:get(prev_r1, Acc)};
        false ->
            Acc#{R1 => true, prev_r1 => R1}
    end;
trace(_Instr, _Regs, _IpVal, _Cycle, Acc) ->
    Acc.

emulate(Prog, IpReg, Regs) ->    
    IpVal = read_reg(IpReg, Regs),
    emulate(Prog, IpReg, IpVal, Regs, 0, #{}).

emulate(Prog, Ip, IpVal, Regs, Cycle, Map) ->    
    case maps:is_key(IpVal, Prog) of
        false ->
            {eop, {cycles, Cycle}, {ip, IpVal}, {regs, Regs}};
        true ->
            %% Write the ip value to the ip register 
            Regs0 = write_reg(Ip, IpVal, Regs),
            
            %% Read the instruction pointed to by the IP
            Instr = {OpCode, A, B, C} = maps:get(IpVal, Prog),

            Map0 = case trace(Instr, Regs, IpVal, Cycle, Map) of
                       {part2, _R1} = X->
                           X;
                       M ->
                           M
                   end,
            
            %% Execute the instruction
            Regs1 = execute_opcode(OpCode, A, B, C, Regs0),
            
            %% Read back any changes to the IP
            IpVal0 = read_reg(Ip, Regs1),
            
            %% Increment the IP
            IpValNext = IpVal0 + 1,
            
            %% Repeat.
            emulate(Prog, Ip, IpValNext, Regs1, Cycle + 1, Map0)
    end.
    
    

opcodes() -> 
    [addr, addi, mulr, muli, banr, bani, borr, bori, setr,
     seti, gtir, gtri, gtrr, eqir, eqri, eqrr].

read_reg(Reg, RegVals) ->
    lists:nth(Reg + 1, RegVals).

write_reg(0, V, [_|RegVals]) ->
    [V|RegVals];
write_reg(N, V, [X|RegVals]) ->
    [X|write_reg(N - 1, V, RegVals)].


%% -- opcode helpers --
execute_arithm_opcode_reg(A, B, C, RegVals, Fun) ->
    AVal = read_reg(A, RegVals),
    BVal = read_reg(B, RegVals),
    write_reg(C, Fun(AVal, BVal), RegVals).

execute_arithm_opcode_i(A, B, C, RegVals, Fun) ->
    AVal = read_reg(A, RegVals),
    write_reg(C, Fun(AVal, B), RegVals).

execute_compare_op(A, B, C, RegVals, Fun) ->
    write_reg(C, case Fun(A, B) of
		     true ->
			 1;
		     _ -> 
			 0
		 end, RegVals).
		
%% -- opcodes --

execute_opcode(addr, A, B, C, RegVals) ->
    execute_arithm_opcode_reg(A, B, C, RegVals, fun plus/2);
execute_opcode(addi, A, B, C, RegVals) -> 
    execute_arithm_opcode_i(A, B, C, RegVals, fun plus/2);

execute_opcode(mulr, A, B, C, RegVals) -> 
    execute_arithm_opcode_reg(A, B, C, RegVals, fun mul/2);
execute_opcode(muli, A, B, C, RegVals) -> 
    execute_arithm_opcode_i(A, B, C, RegVals, fun mul/2);

execute_opcode(banr, A, B, C, RegVals) -> 
    execute_arithm_opcode_reg(A, B, C, RegVals, fun bit_and/2);
execute_opcode(bani, A, B, C, RegVals) -> 
    execute_arithm_opcode_i(A, B, C, RegVals, fun bit_and/2);

execute_opcode(borr, A, B, C, RegVals) -> 
    execute_arithm_opcode_reg(A, B, C, RegVals, fun bit_or/2);
execute_opcode(bori, A, B, C, RegVals) -> 
    execute_arithm_opcode_i(A, B, C, RegVals, fun bit_or/2);

execute_opcode(setr, A, _B, C, RegVals) -> 
    AVal = read_reg(A, RegVals),
    write_reg(C, AVal, RegVals);
execute_opcode(seti, A, _B, C, RegVals) -> 
    write_reg(C, A, RegVals);


execute_opcode(gtri, A, B, C, RegVals) ->
    AVal = read_reg(A, RegVals),
    execute_compare_op(AVal, B, C, RegVals, fun gt/2);
execute_opcode(gtir, A, B, C, RegVals) ->
    BVal = read_reg(B, RegVals),
    execute_compare_op(A, BVal, C, RegVals, fun gt/2);
execute_opcode(gtrr, A, B, C, RegVals) ->
    AVal = read_reg(A, RegVals),
    BVal = read_reg(B, RegVals),
    execute_compare_op(AVal, BVal, C, RegVals, fun gt/2);

execute_opcode(eqri, A, B, C, RegVals) ->
    AVal = read_reg(A, RegVals),
    execute_compare_op(AVal, B, C, RegVals, fun eq/2);
execute_opcode(eqir, A, B, C, RegVals) ->
    BVal = read_reg(B, RegVals),
    execute_compare_op(A, BVal, C, RegVals, fun eq/2);
execute_opcode(eqrr, A, B, C, RegVals) ->
    AVal = read_reg(A, RegVals),
    BVal = read_reg(B, RegVals),
    execute_compare_op(AVal, BVal, C, RegVals, fun eq/2).

%% -- utils --	  

int(S) ->
    list_to_integer(S).

plus(A, B) ->
    A + B.
mul(A, B) ->
    A * B.
bit_and(A, B) ->
    A band B.
bit_or(A, B) ->
    A bor B.
gt(X, Y) ->
    X > Y.
eq(X, Y) ->
    X == Y.
