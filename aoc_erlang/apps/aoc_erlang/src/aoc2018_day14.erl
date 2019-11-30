-module(aoc2018_day14).
-include_lib("eunit/include/eunit.hrl").

%% Chocolate Carts

main_test_() ->
  Input = 633601,
  [ {"Part 1", ?_assertEqual("5115114101", start1(Input))}
  , {"Part 2", timeout, 60,
     ?_assertEqual(20310465, start2(integer_to_list(Input)))}
  ].

start1(Input) ->
  L = <<3, 7>>,
  Bin = step1(L, 0, 1, Input),
  lists:map(fun(X) -> X + $0 end, binary_to_list(Bin)).

step1(L, _, _, Input) when byte_size(L) >= Input + 10 ->
  binary:part(L, Input, 10);
step1(L, Elf1, Elf2, Input) ->
  {L0, NewElf1, NewElf2, _} = append_and_move_elves(L, Elf1, Elf2, Input, false),
  step1(L0, NewElf1, NewElf2, Input).

start2(InputStr) ->
  Bin = list_to_binary(lists:map(fun(X) -> X - $0 end, InputStr)),
  L = <<3, 7>>,
  step2(L, 0, 1, Bin, 0).

%% We know the answer is ~20 million, so stop at 21.
step2(L, _, _, _, _) when byte_size(L) > 21000000 -> false;
step2(_, _, _, _, {found, At}) -> At;
step2(L, Elf1, Elf2, Input, P) ->
  {L0, NewElf1, NewElf2, P0} = append_and_move_elves(L, Elf1, Elf2, Input, P),
  step2(L0, NewElf1, NewElf2, Input, P0).

%%% Helpers

append_and_move_elves(L, Elf1, Elf2, Input, P) ->
  E1 = binary:at(L, Elf1),
  E2 = binary:at(L, Elf2),
  LLen = byte_size(L),

  E12 = E1 + E2,
  {NewP, L0} =
    if E12 < 10 ->
        P1 = progress(P, E12, Input, LLen),
        {P1, <<L/binary, E12>>};
       true ->
        X = 1,
        Y = E12 rem 10,
        P1 = progress(P, X, Input, LLen),
        P2 = progress(P1, Y, Input, LLen + 1),
        {P2, <<L/binary, X, Y>>}
    end,
  Len = byte_size(L0),
  NewElf1 = (Elf1 + E1 + 1) rem Len,
  NewElf2 = (Elf2 + E2 + 1) rem Len,
  {L0, NewElf1, NewElf2, NewP}.

progress(false, _, _, _) -> false;
progress(P, _, _, _) when is_tuple(P) -> P;
progress(P, X, Input, I) when is_integer(P) ->
  NewP = case binary:at(Input, P) == X of
           true -> P + 1;
           false -> 0
         end,
  case NewP of
    L when L == byte_size(Input) ->
      {found, I - byte_size(Input) + 1};
    _ ->
      NewP
  end.


%%% Tests

part1_test() ->
  ?assertEqual("0124515891", start1(5)),
  ?assertEqual("9251071085", start1(18)),
  ?assertEqual("5941429882", start1(2018)).

part2_test() ->
  ?assertEqual(9, start2("51589")),
  %% ?assertEqual(5, start2("01245")),
  ?assertEqual(18, start2("92510")),
  ?assertEqual(2018, start2("59414")).
