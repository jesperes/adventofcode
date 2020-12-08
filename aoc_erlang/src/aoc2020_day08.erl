%%% Created: 2020-12-08T05:38:36+00:00

-module(aoc2020_day08).
-include_lib("eunit/include/eunit.hrl").

%% Puzzle solution
part1(Prog) ->
  {loop, Acc} = execute(Prog),
  Acc.

part2(Prog) ->
  repair(Prog).

%% Types
-type opcode() :: nop
                | acc
                | jmpg.

-type instr() :: {OpCode :: opcode(),
                  Arg :: integer()}.

-type prog() :: #{integer() => instr()}.

-type prog_result() :: {eop, Acc :: integer()}    %% Program finished
                     | {loop, Acc :: integer()}.  %% Program loop detected

%% ======================================================================
%% Part 1: Find value of acc register immediately before an
%% instruction is executed a second time.
%% ======================================================================

-spec execute(Prog :: prog()) -> prog_result().
execute(Prog) ->
  execute(0, 0, Prog, sets:new()).

-spec execute(PC :: integer(),
              Acc :: integer(),
              Prog :: prog(),
              Visited :: sets:set()) ->
        prog_result().
execute(PC, Acc, Prog, Visited) ->
  case maps:is_key(PC, Prog) of
    false ->
      %% Program terminates by jumping outside the range of
      %% instructions
      {eop, Acc};
    true ->
      case sets:is_element(PC, Visited) of
        true ->
          %% If we reach the same instruction twice, we have a loop.
          {loop, Acc};
        false ->
          {NewPC, NewAcc} =
            case maps:get(PC, Prog) of
              {nop, _} -> {PC + 1, Acc};
              {acc, N} -> {PC + 1, Acc + N};
              {jmp, N} -> {PC + N, Acc}
            end,
          execute(NewPC, NewAcc, Prog, sets:add_element(PC, Visited))
      end
  end.

%% ======================================================================
%% Part 2: Find the single jmp/nop which should be flipped to make
%% program terminate. Return value of acc register when program has
%% terminated.
%% ======================================================================

repair(Prog) ->
  repair(Prog, jmp_and_nops(Prog)).

repair(Prog, [{PC, {Instr, N}}|Rest]) ->
  Flipped = flip(Instr),
  RepairedProg = maps:update(PC, {Flipped, N}, Prog),
  case execute(RepairedProg) of
    {loop, _} ->
      repair(Prog, Rest);
    {eop, Acc} ->
      Acc
  end.

%% ======================================================================
%% Helpers
%% ======================================================================

flip(jmp) -> nop;
flip(nop) -> jmp.

jmp_and_nops(Prog) ->
  maps:fold(fun(PC, {Op, _} = Instr, Acc) when
                  Op =:= jmp ;
                  Op =:= nop ->
                [{PC, Instr}|Acc];
               (_PC, _Instr, Acc) ->
                Acc
            end, [], Prog).

%% Input reader (place downloaded input file in
%% priv/inputs/2020/input08.txt).
get_input() ->
  parse_input(inputs:get_as_lines(2020, 08)).

parse_input(Lines) ->
  {_, Prog} =
    lists:foldl(fun(S, {N, Map}) ->
                    [Instr, Op] = string:split(S, " "),
                    {N + 1, maps:put(N, {list_to_atom(Instr),
                                         list_to_integer(Op)}, Map)}
                end, {0, #{}}, Lines),
  Prog.

%% Tests
main_test_() ->
  Input = get_input(),

  [ {"Part 1", ?_assertEqual(1384, part1(Input))}
  , {"Part 2", ?_assertEqual(761, part2(Input))}
  ].

test_input() ->
  ["nop +0",
   "acc +1",
   "jmp +4",
   "acc +3",
   "jmp -3",
   "acc -99",
   "acc +1",
   "jmp -4",
   "acc +6"].

ex1_test_() ->
  ?_assertEqual({loop, 5}, execute(parse_input(test_input()))).


%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
