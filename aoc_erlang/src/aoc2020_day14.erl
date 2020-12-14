%%% Advent of Code solution for 2020 day 14.
%%% Created: 2020-12-14T06:13:49+00:00

-module(aoc2020_day14).
-include_lib("eunit/include/eunit.hrl").

%% Puzzle solution
part1(Input) ->
  Masks = parse(Input),
  Memory = apply_masks(Masks, #{}),
  lists:sum(maps:values(Memory)).

part2(Input) ->
  Masks = parse(Input),
  Memory = apply_masks2(Masks, #{}),
  lists:sum(maps:values(Memory)).

%% ======================================================================
%% Part 1
%% ======================================================================

parse_writes([], Writes) ->
  lists:reverse(Writes);
parse_writes([Write|Rest], Writes) ->
  [<<"mem">>, Addr, Value] = string:lexemes(Write, "[] = "),
  P = {write,
       binary_to_integer(Addr),
       binary_to_integer(Value)},
  parse_writes(Rest, [P|Writes]).

%% TODO rewrite these using foldl
apply_masks([], Memory) ->
  Memory;
apply_masks([{Mask, Writes}|Rest], Memory) ->
  Memory0 = apply_writes(Mask, Writes, Memory),
  apply_masks(Rest, Memory0).

apply_writes(_Mask, [], Memory) ->
  Memory;
apply_writes(Mask, [Write|Rest], Memory) ->
  Memory0 = apply_write(Mask, Write, Memory),
  apply_writes(Mask, Rest, Memory0).

apply_write(Mask, {write, Addr, Value}, Memory) ->
  NewValue = write_bits(Addr, Value, 35, Mask),
  maps:put(Addr, NewValue, Memory).

write_bits(_Addr, Value, _Pos, []) ->
  Value;
write_bits(Addr, Value, Pos, [MaskBit|Rest]) ->
  Value0 =
    case MaskBit of
      $0 -> Value band (bnot (1 bsl Pos));
      $1 -> Value bor (1 bsl Pos);
      $X -> Value
    end,
  write_bits(Addr, Value0, Pos - 1, Rest).

%% ======================================================================
%% Part 2
%% ======================================================================

apply_masks2(Masks, Memory) ->
  lists:foldl(
    fun({Mask, Writes}, MemoryIn) ->
        %% Find which bits are floating
        FloatingBits =
          lists:filtermap(
            fun({Pos, $X}) -> {true, Pos};
               (_) -> false
            end,
            lists:zip(lists:seq(35, 0, -1), Mask)),

        lists:foldl(
          fun({write, Addr, Value}, MemoryIn0) ->
              %% Apply the mask for non-floating bits
              AppliedMask =
                lists:foldl(
                  fun({Pos, $1}, N) -> N bor (1 bsl Pos);
                     (_, N) -> N
                  end, Addr, lists:zip(lists:seq(35, 0, -1), Mask)),

              %% Expand floating writes. The number of writes will be
              %% 2 ** (number of X:s).
              ExpandedWrites = expand_writes(FloatingBits, AppliedMask, Value),

              %% Apply the floating writes
              lists:foldl(fun({write, A, V}, Acc) ->
                              maps:put(A, V, Acc)
                          end, MemoryIn0, ExpandedWrites)
          end, MemoryIn, Writes)
    end, Memory, Masks).

expand_writes(Bits, Addr, Value) ->
  lists:sort(lists:map(fun(N) ->
                           {write, make_expanded_mask(Addr, Bits, N), Value}
                       end, lists:seq(0, 1 bsl (length(Bits)) - 1))).

%% `Addr' is the raw address with the mask (except floating bits)
%% applied. `Bits' are the bit numbers of the floating bits, and `N' is
%% the bitpattern to use.
%%
%% If `Bits` is [3,1,0] and `N' is 011, we should write, into Addr, a
%% 0 at position 3, a 1 at position 1, and a 0 at position 0.
make_expanded_mask(Addr, Bits, N) ->
  lists:foldl(fun({I, Pos}, Acc) ->
                  case (N bsr I) band 1 of
                    1 -> Acc bor (1 bsl Pos);
                    0 -> Acc band (bnot (1 bsl Pos))
                  end
              end, Addr, with_index(Bits)).

with_index(L) ->
  lists:zip(lists:seq(0, length(L) - 1), L).

%% ======================================================================
%% Helpers
%% ======================================================================

filter_empty(L) ->
  lists:filter(fun(<<>>) -> false;
                  (_) -> true
               end, L).

parse(Input) ->
  lists:filtermap(
    fun(Bin) ->
        [First|Rest] = re:split(Bin, "\n", [multiline]),
        Writes = parse_writes(filter_empty(Rest), []),
        {true, {binary_to_list(First), Writes}}
    end, filter_empty(re:split(Input, "mask = ", [multiline]))).


%% Input reader (place downloaded input file in
%% priv/inputs/2020/input14.txt).
get_input() ->
  inputs:get_as_binary(2020, 14).

%% Tests
main_test_() ->
  Input = get_input(),

  [ {"Part 1", ?_assertEqual(8471403462063, part1(Input))}
  , {"Part 2", ?_assertEqual(2667858637669, part2(Input))}
  ].

test_input() ->
  <<"mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X\n",
    "mem[8] = 11\n",
    "mem[7] = 101\n",
    "mem[8] = 0\n">>.

ex1_test_() ->
  ?_assertEqual(165, part1(test_input())).

test_input2() ->
  <<"mask = 000000000000000000000000000000X1001X\n"
    "mem[42] = 100\n"
    "mask = 00000000000000000000000000000000X0XX\n"
    "mem[26] = 1\n">>.

ex2_test_() ->
  ?_assertEqual(208, part2(test_input2())).

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
