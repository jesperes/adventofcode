%%% @author Jesper Eskilson <jesper@eskilson.se>
%%% @copyright (C) 2019, Jesper Eskilson
%%% @doc
%%%
%%% @end
-module(puzzle22).
-export([start/0]).
-include_lib("eunit/include/eunit.hrl").

state() ->
    #{
      %% Boss
      boss_hp => 51,
      boss_damage => 9,

      %% Player
      effects => #{},
      armor => 0,
      hp => 50,
      mana => 500,
      mana_spent => 0,
      last_spell_cast => undef,                 % For debugging
      
      valid => true,                      % If false, state is invalid
      turn => player                      % Next in turn
     }.

spells() ->
    [magic_missile, drain, shield, poison, recharge].

cost(magic_missile) -> 53;
cost(drain) -> 73;
cost(shield) -> 113;
cost(poison) -> 173;
cost(recharge) -> 229.

is_effect(shield) -> true;
is_effect(poison) -> true;
is_effect(recharge) -> true;
is_effect(drain) -> false;
is_effect(magic_missile) -> false.

effect_length(shield) -> 6;
effect_length(poison) -> 6;
effect_length(recharge) -> 5.

%% Problem: what is the least amount of "mana" you can spend and still
%% win the fight.
%%
%% Since there are different spells to cast, this becomes a search
%% problem. Each node in the graph is a tuple of {Player, Boss}.

start() ->
    find_least_mana(10, 250, 13, 8).

find_least_mana(Hp, Mana, BossHP, BossDamage) ->
    State = state(),
    S0 = maps:put(mana, Mana, State),
    S1 = maps:put(hp, Hp, S0),
    S2 = maps:put(boss_hp, BossHP, S1),
    S3 = maps:put(boss_damage, BossDamage, S2),

    {ok, Path} = find_least_mana(S3),
    %% lists:foreach(
    %%   fun(P) ->
    %%           erlang:display(P)
                  
    %%           %% io:format("player = ~p, spell = ~p, mana_spent = ~p, boss_hp = ~p~n",
    %%           %%           [maps:get(turn, P),
    %%           %%            maps:get(spell, P),
    %%           %%            maps:get(mana_spent, P),
    %%           %%            maps:get(boss_hp, P)])
    %%   end, Path),
    [#{mana_spent := ManaSpent}|_] = lists:reverse(Path),
    ManaSpent.

%% 1774 is too high
%% 787 is too low

find_least_mana(Start) ->
    astar:a_star(
      Start,
      fun({neighbors, Current}) ->
              io:format("Searching: ~w~n", [Current]),

              %% Construct a list of all possible moves and their
              %% effects. Remove any possible states where we loose or
              %% overspend mana.
              States = case maps:get(turn, Current) of
                           player ->
                               [player_attack(Current, Spell) || Spell <- spells()];
                           boss ->
                               [boss_attack(Current)]
                       end,
              
              ValidStates = 
                  lists:filter(fun(State) ->
                                       maps:get(valid, State)
                               end, States),

              io:format("Valid neighbors:~n", []),
              lists:foreach(fun(N) ->
                                    io:format("  - ~w~n", [N])
                            end, ValidStates),
              
              ValidStates;
              
         ({dist, Neighbor, Current}) ->
              case maps:get(turn, Current) of
                  boss ->
                      %% Boss turn, no mana spent
                      0;
                  player ->
                      %% The distance from a node to another in this scenario
                      %% is the thing we want to minimize, amount of mana
                      %% spent.  Note that we do not consider mana recharge
                      %% effects as spending "negative" mana. This is probably
                      %% to avoid having search graph edges with negative
                      %% weight.
                      maps:get(mana_spent, Neighbor) - maps:get(mana_spent, Current)
              end;
         ({cost, #{boss_hp := BossHP}}) ->
              %% Cost is used to guide the search, and should return
              %% how "close" the given node is to the goal.
              BossHP;
         ({is_goal, #{boss_hp := BossHP}}) ->
              %% We have reached our goal if the boss's hit points is
              %% 0 or less.
              BossHP =< 0
      end).


%% Do battle according to spell, and return the resulting state.
%%
%% 1. Is spell an effect which is already active -> stop
%%
%% 2. Apply any active effects (from previously cast spells) + remove
%% expired effects.
%%
%% 3. Deduct the spell cost and record amount of mana spent If not
%% enough mana -> stop
%%
%% 4. If regular spell -> apply effect immediately
%%
%% 5. If effect-spell -> add effect + timer to state
%% 
player_attack(State, Spell) ->
    
    %% Is the spell an effect which is already active?
    case is_effect(Spell) of
        true ->
            Effects = maps:get(effects, State),
            Timer = maps:get(Spell, Effects, 0),
            %% If timer is 1, it will be decremented to zero and
            %% expire after taking effect, and can then be applied
            %% again. (Effects which are not active get a Timer value
            %% of 0 here).
            if Timer =< 1 ->
                    player_attack0(State, Spell);
               true ->
                    #{valid => false}
            end;
        false ->
            player_attack0(State, Spell)
    end.

player_attack0(State, Spell) ->
    %% io:format("~n-- ~w turn --~n", [maps:get(turn, State)]),
    %% io:format("- Player has ~p hit points, ~p armor, ~p mana~n",
    %%           [maps:get(hp, State),
    %%            maps:get(armor, State),
    %%            maps:get(mana, State)]),    
    %% io:format("- Boss has ~p hit points~n", [maps:get(boss_hp, State)]),
    %% io:format("Player casts ~p~n", [Spell]),
    
    S0 = maps:put(last_spell_cast, Spell, State),
    S1 = process_all_effects(S0),
    S2 = deduct_spell_cost(S1, Spell),
    case maps:get(valid, S2) of
        false ->
            %% We could not afford the spell.
            S2;
        true ->
            %% Proceed.
            player_attack1(S2, Spell)
    end.

player_attack1(State, Spell) ->
    S0 = 
        case is_effect(Spell) of
            true ->
                %% Add an effect + timer
                maps:update_with(effects,
                                 fun(V) ->
                                         %% We should not be able to
                                         %% cast spells already active
                                         false = maps:is_key(Spell, V),
                                         maps:put(Spell, effect_length(Spell), V)
                                 end, State);
            false ->
                %% Apply effect immediately
                case Spell of 
                    magic_missile ->
                        maps:update_with(boss_hp, fun(V) -> V - 4 end, State);
                    drain ->
                        S1 = maps:update_with(boss_hp, fun(V) -> V - 2 end, State),
                        maps:update_with(hp, fun(V) -> V + 2 end, S1);
                    _ ->
                        State
                end
        end,
    
    maps:put(turn, boss, S0).

boss_attack(State) ->
    S00 = maps:put(last_spell_cast, undef, State),

    %% io:format("~n-- ~w turn --~n", [maps:get(turn, State)]),
    %% io:format("- Player has ~p hit points, ~p armor, ~p mana~n",
    %%           [maps:get(hp, State),
    %%            maps:get(armor, State),
    %%            maps:get(mana, State)]),    
    %% io:format("- Boss has ~p hit points~n", [maps:get(boss_hp, State)]),
    
    %% Process all active effects
    S0 = process_all_effects(S00),
    BossDamage = maps:get(boss_damage, S0),
    PlayerHP = maps:get(hp, S0),

    %% Boss can die here, but this will be detected in the search,
    %% where we are looking for an end state where boss_hp =< 0.
    
    PlayerArmor = maps:get(armor, S0) + 
        case is_active_effect(shield, S0) of
            true -> 7;
            false -> 0
        end,
    
    Damage = max(BossDamage - PlayerArmor, 1),
    S1 = maps:put(hp, PlayerHP - Damage, S0),

    %% Check if player has died.
    PlayerHP0 = maps:get(hp, S1),
    if PlayerHP0 =< 0 ->
            %% If player dies after boss attack, consider state
            %% invalid.
            maps:put(valid, false, S1);
       true ->
            maps:put(turn, player, S1)
    end.


%%% ============================================================
%%% Helper functions
%%% ============================================================


%% Is the given spell currently active?
is_active_effect(Spell, State) ->
    case maps:get(effects, State, 0) of
        #{Spell := Timer} when Timer >= 1 ->
            true;
        _ ->
            false
    end.

process_all_effects(State) ->
    Effects = maps:get(effects, State),

    %% Apply accumulating effects (poison and mana)
    S0 = maps:fold(fun(poison, _, StateIn) ->
                           %% io:format("Poison dealing boss 3 damage.~n", []),
                           maps:update_with(boss_hp, fun(V) -> V - 3 end, StateIn);
                      (recharge, _, StateIn) ->
                           %% io:format("Mana increased by 101.~n", []),
                           maps:update_with(mana, fun(V) -> V + 101 end, StateIn);
                      (shield, _, StateIn) ->
                           StateIn
                   end, State, Effects),
    
    %% Decrement timer 
    M0 = maps:map(fun(_, Timer) -> Timer - 1 end, Effects),
    
    %% lists:foreach(fun({Effect, Timer}) ->
    %%                       io:format("Applying effect ~p, timer is now ~p~n",
    %%                                 [Effect, Timer])
    %%               end,
    %%               maps:to_list(M0)),

    %% Remove expired effects
    E0 = maps:filter(fun(_, Timer) ->
                             Timer >= 1
                     end, M0),

    maps:put(effects, E0, S0).

%% Buy a spell. Record the amount of mana spent and the amount of mana
%% left.
deduct_spell_cost(State, Spell) ->
    S0 = maps:update_with(mana, fun(V) -> V - cost(Spell) end, State),
    S1 = maps:update_with(mana_spent, fun(V) -> V + cost(Spell) end, S0),
    Mana = maps:get(mana, S1),
    if Mana =< 0 ->
            #{valid => false};
       true ->
            S1
    end.

%%% ============================================================
%%% Tests
%%% ============================================================

is_active_effect_test() ->
    ?assert(is_active_effect(shield, #{effects => #{shield => 2}})),
    ?assertNot(is_active_effect(shield, #{effects => #{magic_missile => 0}})),
    ?assertNot(is_active_effect(shield, #{effects => #{shield => 0}})).

deduct_spell_cost_test() ->
    ?assertMatch(#{mana := 47, mana_spent := 53}, 
                 deduct_spell_cost(#{mana => 100, mana_spent => 0}, magic_missile)),
    ?assertMatch(#{valid := false}, 
                 deduct_spell_cost(#{mana => 20, mana_spent => 0}, magic_missile)).

process_all_effects_test() ->
    State = #{ effects => #{poison => 6},
               boss_hp => 100},
    ?assertMatch(#{boss_hp := 97,
                   effects := #{poison := 5}},
                 process_all_effects(State)).
                      
player_attack1_test() ->
    State = #{
              hp => 10,
              mana => 250,
              mana_spent => 0,
              armor => 0,
              boss_hp => 13,
              boss_damage => 8,
              effects => #{},
              valid => true,
              turn => player
             },
    S0 = player_attack(State, poison),

    ?assertMatch(
       #{hp := 10, 
         mana := 77,
         mana_spent := 173,
         boss_hp := 13, %% no change yet, poison only takes effect next turn
         effects := #{ poison := 6 }
        }, S0),
    
    S1 = boss_attack(S0),
    
    ?assertMatch(
       #{hp := 2, 
         mana := 77,
         mana_spent := 173,
         boss_hp := 10, %% poison has reduced boss hp to 10
         effects := #{ poison := 5 }
        }, S1),
    

    S2 = player_attack(S1, magic_missile),
    
    ?assertMatch(
       #{hp := 2, 
         mana := 24,
         mana_spent := 226,
         boss_hp := 3, %% magic_missile + poison deals 4 + 3 == 7 damage
         effects := #{ poison := 4 }
        }, S2),

    S3 = boss_attack(S2),
    
    %% Boss is now dead.
    ?assertMatch(#{boss_hp := 0}, S3).


player_attack2_test() ->
    State = #{
              hp => 10,
              mana => 250,
              mana_spent => 0,
              armor => 0,
              boss_hp => 14,
              boss_damage => 8,
              effects => #{},
              valid => true,
              turn => player
             },

    io:format("~n===============================================~n", []),
    io:format("=== First round ===============================~n", []),
    io:format("===============================================~n", []),
    %% -- Player turn --
    %% - Player has 10 hit points, 0 armor, 250 mana
    %% - Boss has 14 hit points
    %% Player casts Recharge.
    
    %% -- Boss turn --
    %% - Player has 10 hit points, 0 armor, 21 mana
    %% - Boss has 14 hit points
    %% Recharge provides 101 mana; its timer is now 4.
    %% Boss attacks for 8 damage!

    S0 = player_attack(State, recharge),

    ?assertMatch(
       #{hp := 10, 
         mana := 21,
         mana_spent := 229,
         boss_hp := 14,
         effects := #{ recharge := 5 }
        }, S0),

    S1 = boss_attack(S0),
    
    ?assertMatch(
       #{hp := 2, 
         mana := 122,
         mana_spent := 229,
         boss_hp := 14,
         effects := #{ recharge := 4 }
        }, S1),
    
    io:format("===============================================~n", []),
    io:format("=== Second round ==============================~n", []),
    io:format("===============================================~n", []),
    
    %% -- Player turn --
    %% - Player has 2 hit points, 0 armor, 122 mana
    %% - Boss has 14 hit points
    %% Recharge provides 101 mana; its timer is now 3.
    %% Player casts Shield, increasing armor by 7.
    
    %% -- Boss turn --
    %% - Player has 2 hit points, 7 armor, 110 mana
    %% - Boss has 14 hit points
    %% Shield's timer is now 5.
    %% Recharge provides 101 mana; its timer is now 2.
    %% Boss attacks for 8 - 7 = 1 damage!

    S2 = player_attack(S1, shield),
    
    ?assertMatch(
       #{hp := 2, 
         mana := 110,
         mana_spent := 342,
         boss_hp := 14,
         effects := #{ recharge := 3, shield := 6 }
        }, S2),

    S3 = boss_attack(S2),
    
    ?assertMatch(
       #{hp := 1,
         mana := 211,
         mana_spent := 342,
         boss_hp := 14,
         effects := #{ recharge := 2, shield := 5 }
        }, S3),

    io:format("===============================================~n", []),
    io:format("=== Third round ===============================~n", []),
    io:format("===============================================~n", []),
    %% -- Player turn --
    %% - Player has 1 hit point, 7 armor, 211 mana
    %% - Boss has 14 hit points
    %% Shield's timer is now 4.
    %% Recharge provides 101 mana; its timer is now 1.
    %% Player casts Drain, dealing 2 damage, and healing 2 hit points.
    
    %% -- Boss turn --
    %% - Player has 3 hit points, 7 armor, 239 mana
    %% - Boss has 12 hit points
    %% Shield's timer is now 3.
    %% Recharge provides 101 mana; its timer is now 0.
    %% Recharge wears off.
    %% Boss attacks for 8 - 7 = 1 damage!

    S4 = player_attack(S3, drain),
    
    ?assertMatch(
       #{hp := 3, 
         mana := 239,
         mana_spent := 415,
         boss_hp := 12,
         effects := #{ recharge := 1, shield := 4 }
        }, S4),

    S5 = boss_attack(S4),
    
    ?assertMatch(
       #{hp := 2,
         mana := 340,
         mana_spent := 415,
         boss_hp := 12,
         effects := #{ shield := 3 }
        }, S5),

    io:format("===============================================~n", []),
    io:format("=== Fourth round ==============================~n", []),
    io:format("===============================================~n", []),
    %% -- Player turn --
    %% - Player has 2 hit points, 7 armor, 340 mana
    %% - Boss has 12 hit points
    %% Shield's timer is now 2.
    %% Player casts Poison.
    
    %% -- Boss turn --
    %% - Player has 2 hit points, 7 armor, 167 mana
    %% - Boss has 12 hit points
    %% Shield's timer is now 1.
    %% Poison deals 3 damage; its timer is now 5.
    %% Boss attacks for 8 - 7 = 1 damage!
    
    S6 = player_attack(S5, poison),
    
    ?assertMatch(
       #{hp := 2, 
         mana := 167,
         mana_spent := 588,
         boss_hp := 12,
         effects := #{ shield := 2, poison := 6 }
        }, S6),

    S7 = boss_attack(S6),

    ?assertMatch(
       #{hp := 1,
         mana := 167,
         mana_spent := 588,
         boss_hp := 9,
         effects := #{ shield := 1, poison := 5 }
        }, S7),
    
    io:format("==============================================~n", []),
    io:format("=== Fifth round ==============================~n", []),
    io:format("==============================================~n", []),
    %% -- Player turn --
    %% - Player has 1 hit point, 7 armor, 167 mana
    %% - Boss has 9 hit points
    %% Shield's timer is now 0.
    %% Shield wears off, decreasing armor by 7.
    %% Poison deals 3 damage; its timer is now 4.
    %% Player casts Magic Missile, dealing 4 damage.
    
    %% -- Boss turn --
    %% - Player has 1 hit point, 0 armor, 114 mana
    %% - Boss has 2 hit points
    %% Poison deals 3 damage. This kills the boss, and the player wins.

    S8 = player_attack(S7, magic_missile),

    ?assertMatch(
       #{hp := 1, 
         mana := 114,
         mana_spent := 641,
         boss_hp := 2,
         effects := #{ poison := 4 }
        }, S8),

    S9 = boss_attack(S8),
    
    %% Boss is now dead.
    ?assertMatch(#{boss_hp := -1}, S9).
