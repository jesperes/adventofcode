%%% Advent of Code solution for 2019 day 14.
%%% Created: 2019-12-14T07:03:21+00:00

-module(aoc2019_day14).
-include_lib("eunit/include/eunit.hrl").

%% Puzzle solution
part1(Rules) ->
  produce({1, 'FUEL'}, Rules, #{}).

part2(_Rules) ->
  not_implemented.

%% Input reader (place downloaded input file in
%% priv/inputs/2019/input14.txt).
get_input() ->
  inputs:get_as_binary(2019, 14).

parse_input(Binary) ->
  Lines = string:tokens(binary_to_list(Binary), "\n"),
  lists:map(
    fun(Line) ->
        [Left, Right] = string:tokens(Line, "=>"),
        {lists:map(fun parse_chemical/1,
                   string:tokens(Left, ",")),
         produces,
         parse_chemical(Right)}
    end, Lines).

parse_chemical(Str) ->
  [Quantity, Chemical] = string:tokens(Str, " "),
  {list_to_integer(Quantity),
   list_to_atom(Chemical)}.

%% Return the rule needed to produce the given chemical.
find_rules(Chem, Rules) ->
  lists:filter(
    fun({_Req, produces, {_, Chem0}}) ->
        Chem0 =:= Chem
    end, Rules).

produce(Chem, Rules, Inv) ->
  Inv0 = do_produce(Chem, Rules, Inv),
  maps:get(total_ore, Inv0).

%% Produce (at least) the given quantity of chemical.  Returns the
%% updated inventory.
do_produce({Qnty, 'ORE'}, _Rules, Inv) ->
  Inv0 = maps:update_with('ORE', fun(Old) -> Old + Qnty end, Qnty, Inv),
  maps:update_with(total_ore, fun(Old) -> Old + Qnty end, Qnty, Inv0);
do_produce({Qnty, Chem}, Rules, Inv) ->
  %% Compute the amount of chemical we need to produce.
  Inv0 =
    case maps:get(Chem, Inv, 0) of
      Available when Available >= Qnty ->
        %% We already have what we need.
        Inv;
      _Available ->
        %% We need to produce. Start by finding the rule which produces
        %% this chemical, and apply it until we have enough.
        [{_Inputs, produces, _} = Rule] = find_rules(Chem, Rules),
        apply_until(Rule, Rules, Inv)
    end,
  Inv0.

apply_until({Inputs, produces, {Q, C}} = Rule, Rules, Inv) ->

  %% Call do_produce/3 recursively to produce the prerequisites
  %% to this rule.

  Inv0 =
    lists:foldl(fun(Input, Acc) ->
                    do_produce(Input, Rules, Acc)
                end, Inv, Inputs),

  case lists:all(fun({Q0, C0}) ->
                     maps:get(C0, Inv0, 0) >= Q0
                 end, Inputs) of
    true ->
      Inv1 = lists:foldl(
               fun({Q0, C0}, Acc) ->
                   maps:update_with(C0, fun(V) -> V - Q0 end, Acc)
               end, Inv0, Inputs),

      maps:update_with(C, fun(V) -> V + Q end, Q, Inv1);
    false ->
      apply_until(Rule, Rules, Inv0)
  end.

%% Tests
main_test_() ->
  Rules = parse_input(get_input()),
  [ {"Part 1", ?_assertEqual(741927, part1(Rules))}
  , {"Part 2", ?_assertEqual(0, part2(Rules))}
  ].

ex1_test_() ->
  Bin = <<"2 ORE => 1 A\n",
          "1 A => 1 FUEL\n">>,
  Rules = parse_input(Bin),
  ?_assertEqual(2, produce({1, 'FUEL'}, Rules, #{})).

ex2_test_() ->
  Bin = <<"3 ORE => 1 A\n",
          "5 A => 1 FUEL\n">>,
  Rules = parse_input(Bin),
  ?_assertEqual(15, produce({1, 'FUEL'}, Rules, #{})).

ex3_test_() ->
  Bin = <<"9 ORE => 2 A\n",
          "8 ORE => 3 B\n",
          "7 ORE => 5 C\n",
          "3 A, 4 B => 1 AB\n",
          "5 B, 7 C => 1 BC\n",
          "4 C, 1 A => 1 CA\n",
          "2 AB, 3 BC, 4 CA => 1 FUEL\n">>,
  Rules = parse_input(Bin),
  ?_assertEqual(165, produce({1, 'FUEL'}, Rules, #{})).

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
