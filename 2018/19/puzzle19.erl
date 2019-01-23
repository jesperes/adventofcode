%%% @author Jesper Eskilson <>
%%% @copyright (C) 2018, Jesper Eskilson
%%% @doc
%%%
%%% @end
%%% Created : 19 Dec 2018 by Jesper Eskilson <>

-module(puzzle19).
-export([main/0]).

main() ->
    {eop, _, {regs, [R0|_]}} = start(),
    {{part1, R0},
     {part2, start2()}}.
      
start() ->
    test(),
    {Ip, Prog} = read_program("input.txt"),
    emulate(Prog, Ip).

test() ->
    [0, 1, 42, 3, 4, 5] = write_reg(2, 42, [0, 1, 2, 3, 4, 5]).

magic_number() ->
    10551398.

initregs() ->
    [0, 0, 0, 0, 0, 0].

%% Part 2 (where R1 is set to 1) computes the sum of the factors of
%% the magic_number(), so we solve directly instead.
start2() ->
    Factors = start2(1, magic_number(), []),
    lists:foldl(fun plus/2, 0, Factors).

start2(N, Magic, Factors) when Magic rem N == 0 ->
    [N|start2(N + 1, Magic, Factors)];
start2(N, Magic, Factors) when N < Magic ->
    start2(N + 1, Magic, Factors);
start2(_, _, Factors) ->
    Factors.

read_program(Filename) ->
    {ok, Binary} = file:read_file(Filename),
    ProgAsStr = binary_to_list(Binary),
    [IpLine|Lines] = string:tokens(ProgAsStr, "\n\r"),
    ["#ip", IpStr] = string:tokens(IpLine, " "),
    Ip = list_to_integer(IpStr),
    {_, Map} = 
        lists:foldl(fun(X, {N, Map}) ->
                            Tokens = string:tokens(X, " "),
                            {N + 1, maps:put(N, parse_line(Tokens), Map)}
                    end, {0, #{}}, Lines),
    {Ip, Map}.
       

parse_line([Op, A, B, C]) ->
    {list_to_atom(Op), 
     list_to_integer(A),
     list_to_integer(B),
     list_to_integer(C)}.

emulate(Prog, IpReg) ->    
    Regs = initregs(),
    IpVal = read_reg(IpReg, Regs),
    emulate(Prog, IpReg, IpVal, Regs, 0).

emulate(Prog, Ip, IpVal, Regs, Cycle) ->
    %% if (Cycle rem 1000000 == 0) ->
    %%         io:format("Cycle: ~p IP: ~02s  Regs: ~p~n", [Cycle, integer_to_list(IpVal), Regs]);
    %%    true ->
    %%         ok
    %% end,
    
    case maps:is_key(IpVal, Prog) of
        false ->
            {eop, {ip, IpVal}, {regs, Regs}};
        true ->
            %% Write the ip value to the ip register 
            Regs0 = write_reg(Ip, IpVal, Regs),
            
            %% Read the instruction pointed to by the IP
            {OpCode, A, B, C} = maps:get(IpVal, Prog),
            
            %% Execute the instruction
            Regs1 = execute_opcode(OpCode, A, B, C, Regs0),
            
            %% Read back any changes to the IP
            IpVal0 = read_reg(Ip, Regs1),
            
            %% Increment the IP
            IpValNext = IpVal0 + 1,
            
            %% Repeat.
            emulate(Prog, Ip, IpValNext, Regs1, Cycle + 1)
    end.
    
    

%%opcodes() -> 
%%    [addr, addi, mulr, muli, banr, bani, borr, bori, setr,
%%     seti, gtir, gtri, gtrr, eqir, eqri, eqrr].

read_reg(0, [R0|_]) -> R0;
read_reg(1, [_, R1|_]) -> R1;
read_reg(2, [_, _, R2|_]) -> R2;
read_reg(3, [_, _, _, R3|_]) -> R3;
read_reg(4, [_, _, _, _, R4|_]) -> R4;
read_reg(5, [_, _, _, _, _, R5]) -> R5.

write_reg(0, V, [_, R1, R2, R3, R4, R5]) -> [V, R1, R2, R3, R4, R5];
write_reg(1, V, [R0, _, R2, R3, R4, R5]) -> [R0, V, R2, R3, R4, R5];
write_reg(2, V, [R0, R1, _, R3, R4, R5]) -> [R0, R1, V, R3, R4, R5];
write_reg(3, V, [R0, R1, R2, _, R4, R5]) -> [R0, R1, R2, V, R4, R5];
write_reg(4, V, [R0, R1, R2, R3, _, R5]) -> [R0, R1, R2, R3, V, R5];
write_reg(5, V, [R0, R1, R2, R3, R4, _]) -> [R0, R1, R2, R3, R4, V].

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


execute_opcode(gtir, A, B, C, RegVals) ->
    AVal = read_reg(A, RegVals),
    execute_compare_op(AVal, B, C, RegVals, fun gt/2);
execute_opcode(gtri, A, B, C, RegVals) ->
    BVal = read_reg(B, RegVals),
    execute_compare_op(A, BVal, C, RegVals, fun gt/2);
execute_opcode(gtrr, A, B, C, RegVals) ->
    AVal = read_reg(A, RegVals),
    BVal = read_reg(B, RegVals),
    execute_compare_op(AVal, BVal, C, RegVals, fun gt/2);

execute_opcode(eqir, A, B, C, RegVals) ->
    AVal = read_reg(A, RegVals),
    execute_compare_op(AVal, B, C, RegVals, fun eq/2);
execute_opcode(eqri, A, B, C, RegVals) ->
    BVal = read_reg(B, RegVals),
    execute_compare_op(A, BVal, C, RegVals, fun eq/2);
execute_opcode(eqrr, A, B, C, RegVals) ->
    AVal = read_reg(A, RegVals),
    BVal = read_reg(B, RegVals),
    execute_compare_op(AVal, BVal, C, RegVals, fun eq/2).

%% -- utils --	  

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
