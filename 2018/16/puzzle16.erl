%%%-------------------------------------------------------------------
%%% @author  <jespe@LAPTOP-P6HKA27J>
%%% @copyright (C) 2018, 
%%% @doc
%%%
%%% @end
%%% Created : 16 Dec 2018 by  <jespe@LAPTOP-P6HKA27J>
%%%-------------------------------------------------------------------
-module(puzzle16).

-export([start/0]).

start() ->
    {ok, Binary} = file:read_file("input.txt"),
    Lines = string:tokens(binary_to_list(Binary), "\n"),
    List = parse_lines(Lines),
    P = pass2(List),
    
    MatchingInstructions =
	lists:map(
	  fun({[before|RegValsBefore], [OpNum,A,B,C] = Instr, ['after'|RegValsAfter]}) ->
		  {sample, RegValsBefore, Instr, RegValsAfter, matches,
		   lists:filtermap(fun({OpCode, RegValsAfter0}) ->
					   case RegValsAfter0 == RegValsAfter of
					       true ->
						   {true, OpCode};
					       false ->
						   false
					   end
				   end, 
				   [{OpCode, execute_opcode(OpCode, A, B, C, RegValsBefore)}
				    || OpCode <- opcodes()])}
	  end, P),
    
    length(
      lists:filter(fun({sample, _, _, _, _, OpCodes}) ->
			   length(OpCodes) >= 3
		   end, MatchingInstructions)).
	     			
    
   

parse_lines([]) ->
    [];
parse_lines([Line|Lines]) ->
    Tokens = string:tokens(Line, ": [,]"),
    [parse_line(Tokens)|parse_lines(Lines)].

parse_line(Tokens) ->
    case Tokens of 
	["Before", R1, R2, R3, R4] ->
	    [before, int(R1), int(R2), int(R3), int(R4)];
	[OpCode, A, B, C] ->
	    [int(OpCode), int(A), int(B), int(C)];
	["After", R1, R2, R3, R4] ->
	    ['after', int(R1), int(R2), int(R3), int(R4)]
    end.

pass2([]) ->
    [];
pass2([[before|_] = Before, Instr, ['after'|_] = After|Rest]) ->
    [{Before, Instr, After}|pass2(Rest)];
pass2([_|Rest]) ->
    pass2(Rest).

opcodes() -> 
    [addr, addi, mulr, muli, banr, bani, borr, bori, setr,
     seti, gtir, gtri, gtrr, eqir, eqri, eqrr].



read_reg(Reg, RegVals) ->
    lists:nth(Reg + 1, RegVals).

write_reg(0, Val, [_R1,R2,R3,R4]) ->
    [Val,R2,R3,R4];
write_reg(1, Val, [R1,_R2,R3,R4]) ->
    [R1,Val,R3,R4];
write_reg(2, Val, [R1,R2,_R3,R4]) ->
    [R1,R2,Val,R4];
write_reg(3, Val, [R1,R2,R3,_R4]) ->
    [R1,R2,R3,Val].

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
