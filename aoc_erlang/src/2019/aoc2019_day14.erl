%%% Advent of Code solution for 2019 day 14.
%%% Created: 2019-12-14T07:03:21+00:00

-module(aoc2019_day14).
-include_lib("eunit/include/eunit.hrl").

%% Puzzle solution
part1(Rules) ->
  produce(1, 'FUEL', Rules).

part2(Rules) ->
  MaxOre = 1000000000000,
  binary_search(1, inf, MaxOre, Rules).

%% Binary search exploring the upper limit by starting at 1 and
%% doubling until we overshoot.
binary_search(Lower, inf = Upper, MaxOre, Rules) ->
  case produce(Lower, 'FUEL', Rules) of
    TotalOre when TotalOre < MaxOre ->
      binary_search(Lower * 2, Upper, MaxOre, Rules);
    _ ->
      binary_search(floor(Lower / 2), Lower, MaxOre, Rules)
  end;
binary_search(Lower, Upper, MaxOre, Rules) when Lower < Upper ->
  case floor(Lower + (Upper - Lower)/2) of
    %% If middle == lower, it means that upper exceed the limit, but
    %% middle does not.
    Middle when Middle == Lower -> Middle;
    Middle ->
      TotalOre = produce(Middle, 'FUEL', Rules),
      if TotalOre > MaxOre ->
          binary_search(Lower, Middle, MaxOre, Rules);
         true ->
          binary_search(Middle, Upper, MaxOre, Rules)
      end
  end.

%% Input reader (place downloaded input file in
%% priv/inputs/2019/input14.txt).
get_input() ->
  inputs:get_as_binary(2019, 14).

parse_input(Binary) ->
  Lines = string:tokens(binary_to_list(Binary), "\n"),
  maps:from_list(
    lists:map(
      fun(Line) ->
          [Left, Right] = string:tokens(Line, "=>"),
          {_Q, C} = parse_chemical(Right),
          {C, {lists:map(fun parse_chemical/1,
                         string:tokens(Left, ",")),
               produces,
               parse_chemical(Right)}}
      end, Lines)).

parse_chemical(Str) ->
  [Quantity, Chemical] = string:tokens(Str, " "),
  {list_to_integer(Quantity),
   list_to_atom(Chemical)}.

%% Produce a given amount of chemical. Returns the total amount of ORE
%% used.
produce(Qnty, Chem, Rules) ->
  Inv0 = do_produce({Qnty, Chem}, Rules, #{}),
  maps:get(total_ore, Inv0).

%% Recursive entry point. Produce (at least) the given amount of
%% chemical given an inventory.
do_produce({Qnty, 'ORE'}, _, Inv) ->
  Inv0 = maps:update_with('ORE', fun(Old) -> Old + Qnty end, Qnty, Inv),
  Inv1 = maps:update_with(total_ore, fun(Old) -> Old + Qnty end, Qnty, Inv0),
  Inv1;
do_produce({Qnty, Chem}, Rules, Inv) ->
  case maps:get(Chem, Inv, 0) of
    Available when Available >= Qnty -> Inv;
    Available ->
      {_, produces, {Q, _}} = Rule = maps:get(Chem, Rules),
      Needed = Qnty - Available,
      Repeats = ceil(Needed / Q),
      apply_rule(Rule, Repeats, Rules, Inv)
  end.

%% Apply a rule to produce a chemical
%% @param Rule       The rule to produce
%% @param Multiplier How many copies of the rule to apply
%% @param Rules      The rules, needed to recurse when producing inputs.
%% @param Inv        Inventory
apply_rule({Inputs, produces, {Q, C}} = Rule, Multiplier, Rules, Inv) ->
  M = fun(X) -> X * Multiplier end,
  case lists:all(fun({Q0, C0}) ->
                     maps:get(C0, Inv, 0) >= M(Q0)
                 end, Inputs) of
    true ->
      Inv0 = lists:foldl(
               fun({Q0, C0}, Acc) ->
                   maps:update_with(C0, fun(V) -> V - M(Q0) end, Acc)
               end, Inv, Inputs),
      maps:update_with(C, fun(V) -> V + M(Q) end, M(Q), Inv0);

    false ->
      Inv0 =
        lists:foldl(fun({Qin, Cin}, Acc) ->
                        do_produce({M(Qin), Cin}, Rules, Acc)
                    end, Inv, Inputs),
      apply_rule(Rule, Multiplier, Rules, Inv0)
  end.

%% Tests
main_test_() ->
  Rules = parse_input(get_input()),
  [ {"Part 1", ?_assertEqual(741927, part1(Rules))}
  , {"Part 2", ?_assertEqual(2371699, part2(Rules))}
  ].

ex1_test_() ->
  Bin = <<"2 ORE => 1 A\n",
          "1 A => 1 FUEL\n">>,
  Rules = parse_input(Bin),
  ?_assertEqual(2, part1(Rules)).

ex2_test_() ->
  Bin = <<"3 ORE => 1 A\n",
          "5 A => 1 FUEL\n">>,
  Rules = parse_input(Bin),
  ?_assertEqual(15, part1(Rules)).

ex3_test_() ->
  Bin = <<"9 ORE => 2 A\n",
          "8 ORE => 3 B\n",
          "7 ORE => 5 C\n",
          "3 A, 4 B => 1 AB\n",
          "5 B, 7 C => 1 BC\n",
          "4 C, 1 A => 1 CA\n",
          "2 AB, 3 BC, 4 CA => 1 FUEL\n">>,
  Rules = parse_input(Bin),
  ?_assertEqual(165, part1(Rules)).

ex4_test_() ->
  Bin = <<"157 ORE => 5 NZVS\n",
          "165 ORE => 6 DCFZ\n",
          "44 XJWVT, 5 KHKGT, 1 QDVJ, 29 NZVS, 9 GPVTF, 48 HKGWZ => 1 FUEL\n",
          "12 HKGWZ, 1 GPVTF, 8 PSHF => 9 QDVJ\n",
          "179 ORE => 7 PSHF\n",
          "177 ORE => 5 HKGWZ\n",
          "7 DCFZ, 7 PSHF => 2 XJWVT\n",
          "165 ORE => 2 GPVTF\n",
          "3 DCFZ, 7 NZVS, 5 HKGWZ, 10 PSHF => 8 KHKGT\n">>,
  Rules = parse_input(Bin),
  [ ?_assertEqual(13312, part1(Rules))
  , ?_assertEqual(82892753, part2(Rules))
  ].

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
