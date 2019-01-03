%%% @author Jesper Eskilson <jesper.eskilson@klarna.com>
%%% @copyright (C) 2019, Jesper Eskilson
%%% @doc
%%%
%%% @end
%%% Created :  2 Jan 2019 by Jesper Eskilson <jesper.eskilson@klarna.com>

-module(puzzle22_alt).
-export([start/0, start1/0]).
-include_lib("eunit/include/eunit.hrl").

%% Alternative approach to day 22

%% 494 is not correct
%% 1428 is not correct

spells() ->
    [magic_missile, drain, shield, poison, recharge].

cost(magic_missile) -> 53;
cost(drain) -> 73;
cost(shield) -> 113;
cost(poison) -> 173;
cost(recharge) -> 229.

effect_length(shield) -> 6;
effect_length(poison) -> 6;
effect_length(recharge) -> 5.

state(HP, Mana, BossHP, BossDamage, Spells) ->
    #{
      boss_hp => BossHP,
      boss_damage => BossDamage,
      effects => #{},
      hp => HP,
      mana => Mana,
      mana_spent => 0,
      armor => 0,
      recursion_level => 0,
      winner => false,
      spells => Spells
     }.

start() ->
    start(50, 250, 14, 8, search).

start1() ->   
    start(50, 500, 51, 9, search).

part1_1_test() ->
    Result = start(10, 250, 13, 8, [poison, magic_missile]),
    ?assertEqual(226, Result).

part1_2_test() ->
    Result = start(10, 250, 14, 8, [recharge, shield, drain, poison, magic_missile]),
    ?assertEqual(641, Result).

part1_3_test() ->
    Result = start(10, 250, 14, 8, search),
    ?assertEqual(641, Result).
    
start(HP, Mana, BossHP, BossDamage, Spells) ->
    InitState = state(HP, Mana, BossHP, BossDamage, Spells),
    battle(player, InitState).

%% Valid spells are spells which are not currently in effect, and ones
%% which we can afford.
valid_spells(State) ->
    Effects = maps:get(effects, State),
    Mana = maps:get(mana, State),

    lists:filter(fun(Spell) ->
                         (not maps:is_key(Spell, Effects))
                             and (cost(Spell) =< Mana)
                 end, spells()).

incr_rec(State) ->
    State#{recursion_level => maps:get(recursion_level, State) + 1}.

print_indent(State, Format, Args) ->
    RecLevel = maps:get(recursion_level, State),
    io:format("~*s(~w) ~s",
              [RecLevel * 2, "", RecLevel, io_lib:format(Format, Args)]),
    ok.
    

%% Returns the best mana found, or 'undef' if no valid solution.
battle(boss, #{boss_hp := BossHP} = State) when BossHP =< 0 ->
    %% erlang:display({player_wins_direct, BossHP, maps:get(mana_spent, State)}),    
    print_indent(State, "*** PLAYER WINS (boss hp = ~w, lowest mana spent = ~w) ***~n", 
                 [BossHP, 
                  maps:get(mana_spent, State)]),
    maps:get(mana_spent, State);
battle(player, #{hp := HP} = State) when HP =< 0 ->
    print_indent(State, "*** PLAYER LOSES (hp = ~w) ***~n", [HP]),
    undef;
battle(player, #{spells := []}) ->
    undef;
battle(player, State) ->
    S0 = incr_rec(State),

    print_indent(S0, "Player turn, hp = ~p, mana = ~p, boss_hp = ~p~n", 
                 [maps:get(hp, S0),
                  maps:get(mana, S0),
                  maps:get(boss_hp, S0)]),
    S1 = apply_effects(S0),

    case maps:get(spells, S1) of
        search ->
            ValidSpells = valid_spells(S1),
            
            print_indent(S0, "Considering ~w spells at this level: ~w~n", 
                         [length(ValidSpells), ValidSpells]),

            States = lists:map(fun(Spell) ->
                                       apply_spell(Spell, S1)
                               end, ValidSpells),

            Best = 
                lists:foldl(
                  fun(S, undef) ->
                          Spell = maps:get(spell_cast, S),
                          print_indent(S0, "No known solution at this level yet, battling with '~w'...~n", 
                                       [Spell]),
                          battle(boss, S);       
                                          
                     (S, BestMana) ->
                          Spell = maps:get(spell_cast, S),
                          print_indent(S0, "Battling with '~w'...~n", [Spell]),
                          min(BestMana, battle(boss, S))
                  end,
                  undef,
                  States
                 ),

            %% This case is for debugging only
            case Best of
                undef ->
                    print_indent(S0, "No winning solutions found for any spell at this level.~n",[]),
                    undef;
                N when is_number(N) ->
                    print_indent(S0, "Propagating best solution upwards: ~w~n", [N]),
                    N
            end;

        %% This is the mode where we have a list of spells to apply,
        %% in this case every state is a "winner".
        [Spell|Rest] ->
            S2 = apply_spell(Spell, S1),
            S3 = maps:put(spells, Rest, S2),
            battle(boss, S3)
    end;

battle(boss, State) ->
    S0 = incr_rec(State),
    print_indent(S0, "Boss turn (hp = ~p, boss_hp = ~p)~n", 
                 [maps:get(hp, S0),
                  maps:get(boss_hp, S0)]),

    S1 = apply_effects(S0),

    %% Check if boss has died as a result of spell effects.
    case maps:get(boss_hp, S1) of
        HP when HP =< 0 ->
            %% This is one of the possible end conditions: the boss is
            %% killed by a player effect on the boss turn.

            %% erlang:display({player_wins_indirect, HP, maps:get(mana_spent, State)}),
            print_indent(S0, "*** PLAYER WINS (boss hp = ~w)~n", [HP]),
            maps:get(mana_spent, S1);
        _ ->
            %% The logic here is much simpler than for the player
            %% since we have no choices to make, and no mana spending
            %% to keep track of.
            BossDamage = maps:get(boss_damage, S1) - maps:get(armor, S1),
            PlayerHP = maps:get(hp, S1),
            print_indent(S0, "Boss deals ~w damage to player~n", [BossDamage]),
            S2 = maps:put(hp, PlayerHP - BossDamage, S1),
            S3 = maps:put(spell_cast, undef, S2),
            battle(player, S3)
    end.
    
apply_effects(State) ->
    Effects = maps:get(effects, State),

    %% Apply effects
    S0 = maps:fold(fun(poison, _T, StateIn) ->
                           %%print_indent(State, "Poison dealing 3 damage to boss.~n", []),
                           maps:update_with(boss_hp, fun(V) -> V - 3 end, StateIn);
                      (recharge, _T, StateIn) ->
                           %%print_indent(State, "Poison increasing mana by 101.~n", []),
                           maps:update_with(mana, fun(V) -> V + 101 end, StateIn);
                      (shield, _T, StateIn) ->
                           %%print_indent(State, "Shield setting armor to 7.~n", []),
                           maps:put(armor, 7, StateIn)
                   end, State, Effects),
    
    %% Decrement timers
    Effects0 = maps:map(fun(_, Timer) -> Timer - 1 end, Effects),

    %% Decrease armor to 0 when shield spell expires.
    S1 = case maps:get(shield, Effects0, 0) of
             Timer when Timer =< 0 ->
                 maps:put(armor, 0, S0);
             _ ->
                 S0
         end,

    %% Prune expired timers
    Effects1 = maps:filter(fun(_Spell, Timer) -> 
                                   if Timer >= 1 ->
                                           true;
                                      true ->
                                           false
                                   end
                           end, Effects0),

    %% print_indent(State, "Applied effects ~w -> ~w~n", [Effects, Effects1]),

    maps:put(effects, Effects1, S1).


create_effect(State, Spell) ->
    maps:update_with(effects,
                     fun(V) -> maps:put(Spell, effect_length(Spell), V) end, 
                     State).

apply_spell(Spell, State) ->
    BossHP = maps:get(boss_hp, State),
    HP = maps:get(hp, State),

    S0 = 
        case Spell of
            magic_missile ->
                maps:put(boss_hp, BossHP - 4, State);
            drain ->
                S1 = maps:put(boss_hp, BossHP - 2, State),
                maps:put(hp, HP + 2, S1);
            shield ->                  
                create_effect(State, shield);
            poison ->                  
                create_effect(State, poison);
            recharge ->                  
                create_effect(State, recharge)    
        end,
    
    S2 = maps:update_with(mana, fun(V) -> V - cost(Spell) end, S0),
    S3 = maps:update_with(mana_spent, fun(V) -> V + cost(Spell) end, S2),
    
    maps:put(spell_cast, Spell, S3).
