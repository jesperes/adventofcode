%%% @author  <jespe@LAPTOP-P6HKA27J>
%%% @copyright (C) 2018,
%%% @doc
%%%
%%% @end
%%% Created : 31 Dec 2018 by  <jespe@LAPTOP-P6HKA27J>

-module(puzzle21).
-export([start/0]).
-include_lib("eunit/include/eunit.hrl").

-record(combatant, {
	  damage,
	  armor,
	  hp,
	  cost,
	  name
	 }).

shop_inventory() ->
    #{dagger 		=> #{cost => 8,   damage => 4, type => weapon },
      shortsword 	=> #{cost => 10,  damage => 5, type => weapon },
      warhammer		=> #{cost => 25,  damage => 6, type => weapon },
      longsword		=> #{cost => 40,  damage => 7, type => weapon },
      greataxe		=> #{cost => 74,  damage => 8, type => weapon },

      leather 		=> #{cost => 13,  armor => 1,  type => armor },
      chainmail		=> #{cost => 31,  armor => 2,  type => armor },
      splintmail	=> #{cost => 53,  armor => 3,  type => armor },
      bandedmail 	=> #{cost => 75,  armor => 4,  type => armor },
      platemail		=> #{cost => 102, armor => 5,  type => armor },

      damage1		=> #{cost => 25,  damage => 1, type => ring },
      damage2		=> #{cost => 50,  damage => 2, type => ring },
      damage3		=> #{cost => 100, damage => 3, type => ring },
      defense1		=> #{cost => 20,  armor  => 1, type => ring },
      defense2		=> #{cost => 40,  armor  => 2, type => ring },
      defense3		=> #{cost => 80,  armor  => 3, type => ring }
      }.

weapons() -> [dagger, shortsword, warhammer, longsword, greataxe].
rings() -> [damage1, damage2, damage3, defense1, defense2, defense3].
armors() -> [leather, chainmail, splintmail, bandedmail, platemail].

combinations([]) ->
    [];
combinations([H | T]) ->
    CT = combinations(T),
    [[H]] ++ [[H | L] || L <- CT] ++ CT.

boss() ->
    #combatant{hp = 100,
	       damage = 8,
	       armor = 2,
	       name = boss
	      }.

player({Weapon, Rings, Armor}) ->
    Inv = shop_inventory(),

    %% Exactly one weapon is selected
    WeaponInfo = maps:get(Weapon, Inv),
    WeaponDamage = maps:get(damage, WeaponInfo),
    WeaponCost = maps:get(cost, WeaponInfo),

    %% 0-2 rings can be selected, and can yield both armor and damage.
    {RingDamage, RingArmor, RingCost} =
	lists:foldl(fun(Ring, {DamageIn, ArmorIn, CostIn}) ->
			    RingInfo = maps:get(Ring, Inv),
			    D = maps:get(damage, RingInfo, 0),
			    A = maps:get(armor, RingInfo, 0),
			    C = maps:get(cost, RingInfo),
			    {DamageIn + D, ArmorIn + A, CostIn + C}
		    end, {0, 0, 0}, Rings),

    %% Armor is optional.
    {ArmorArmor, ArmorCost} =
	case Armor of
	    [] -> {0, 0};
	    [A] ->
		ArmorInfo = maps:get(A, Inv),
		{maps:get(armor, ArmorInfo),
		 maps:get(cost, ArmorInfo)}
	end,

    #combatant{hp = 100,
	       damage = WeaponDamage + RingDamage,
	       armor = ArmorArmor + RingArmor,
	       cost = WeaponCost + ArmorCost + RingCost,
	       name = player
	       }.

start() ->
    {part1(), part2()}.

part1() ->
    Combos =
	[{Weapon, Rings, Armor} ||
	    Weapon <- weapons(),
	    Rings <- combinations(rings()) ++ [[]],
	    length(Rings) =< 2,
	    Armor <- [[A] || A <- armors()] ++ [[]]],

    %% Battle the boss using all combinations, and filter out the ones
    %% where we win.
    WinningCombos =
	lists:filtermap(fun(Equipment) ->
				Boss = boss(),
				Player = player(Equipment),
				case battle(Player, Boss) of
				    {winner, player} ->
					{true, {Player, Equipment}};
				    _ ->
					false
				end
			end, Combos),

    %% Find the combo with lowest cost
    {Cost, _, _} = 
        lists:foldl(fun({#combatant{cost = Cost} = Player, Equipment}, {OldMin, _, _})
                          when Cost < OldMin ->
                            {Cost, Player, Equipment};
                       (_, Acc) ->
                            Acc
                    end, {10000000, undef, undef}, WinningCombos),
    Cost.

part2() ->
    Combos =
	[{Weapon, Rings, Armor} ||
	    Weapon <- weapons(),
	    Rings <- combinations(rings()) ++ [[]],
	    length(Rings) =< 2,
	    Armor <- [[A] || A <- armors()] ++ [[]]],

    %% Battle the boss using all combinations, and filter out the ones
    %% where we lose.
    LosingCombos =
	lists:filtermap(fun(Equipment) ->
				Boss = boss(),
				Player = player(Equipment),
				case battle(Player, Boss) of
				    {winner, boss} ->
					{true, {Player, Equipment}};
				    _ ->
					false
				end
			end, Combos),

    %% Find the combo with lowest cost
    {Cost, _, _} = 
        lists:foldl(fun({#combatant{cost = Cost} = Player, Equipment}, {OldMax, _, _})
                          when Cost > OldMax ->
                            {Cost, Player, Equipment};
                       (_, Acc) ->
                            Acc
                    end, {0, undef, undef}, LosingCombos),
    Cost.


battle(Attacker, #combatant{hp = HP} = Defender) ->
    Damage = damage_dealt(Attacker, Defender),
    Def0 = Defender#combatant{hp = HP - Damage},
    %% io:format("The ~p deals ~p damage, the ~p goes down to ~p hit points.~n",
    %% 	      [Attacker#combatant.name,
    %% 	       Damage,
    %% 	       Def0#combatant.name,
    %% 	       Def0#combatant.hp]),
    if Def0#combatant.hp =< 0 ->
	    {winner, Attacker#combatant.name};
       true ->
	    battle(Def0, Attacker)
    end.

damage_dealt(Attacker, Defender) ->
    %% Damage dealt by an attacker each turn is equal to the
    %% attacker's damage score minus the defender's armor score. An
    %% attacker always does at least 1 damage.
    max(Attacker#combatant.damage - Defender#combatant.armor, 1).

%%% ============================================================
%%% Unit tests
%%% ============================================================

damage_dealt_test_() ->
    [
     ?_assertEqual(5, damage_dealt(#combatant{damage = 8}, #combatant{armor = 3})),
     ?_assertEqual(1, damage_dealt(#combatant{damage = 8}, #combatant{armor = 100}))
    ].


battle_test() ->
    Player = #combatant{hp = 8, damage = 5, armor = 5, name = player},
    Boss = #combatant{hp = 12, damage = 7, armor = 2, name = boss},
    ?assertEqual({winner, player}, battle(Player, Boss)).
