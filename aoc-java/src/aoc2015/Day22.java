package aoc2015;

import static org.junit.Assert.assertEquals;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import org.junit.Test;

public class Day22 {
    final static int HP = 51;
    final static int DAMAGE = 9;

    enum Spell {
        MagicMissile(53, 0) {
            @Override
            public void applySpell(State state) {
                state.bossHp -= 4;
            }
        },
        Drain(73, 0) {
            @Override
            public void applySpell(State state) {
                state.bossHp -= 2;
                state.hp += 2;
            }
        },
        Shield(113, 6) {
            @Override
            public void applySpell(State state) {
                state.activeEffects.put(this, effectLen);
            }

            @Override
            public void applyEffect(State state) {
                state.armor = 7;
            }
        },
        Poison(173, 6) {
            @Override
            public void applySpell(State state) {
                state.activeEffects.put(this, effectLen);
            }

            @Override
            public void applyEffect(State state) {
                state.bossHp -= 3;
            }
        },
        Recharge(229, 5) {
            @Override
            public void applySpell(State state) {
                state.activeEffects.put(this, effectLen);
            }

            @Override
            public void applyEffect(State state) {
                state.mana += 101;
            }
        };

        int cost;
        int effectLen;

        Spell(int cost, int effectLen) {
            this.cost = cost;
            this.effectLen = effectLen;
        }

        public void applySpell(State state) {
        }

        public void applyEffect(State state) {
        }
    }

    static class State {
        int hp;
        int mana;
        int manaSpent;
        int bossHp;
        int bossDamage;
        int armor;
        int hpPenalty;
        Map<Spell, Integer> activeEffects = new HashMap<>();

        public State(int hp, int mana, int manaSpent, int bossHp,
                int bossDamage, int armor, int hpPenalty,
                Map<Spell, Integer> activeEffects) {
            this.hp = hp;
            this.mana = mana;
            this.manaSpent = manaSpent;
            this.bossHp = bossHp;
            this.bossDamage = bossDamage;
            this.armor = armor;
            this.hpPenalty = hpPenalty;
            this.activeEffects = new HashMap<>(activeEffects);
        }

        public State(int hp, int mana) {
            this.hp = hp;
            this.mana = mana;
            this.manaSpent = 0;
            this.bossHp = HP;
            this.bossDamage = DAMAGE;
            this.armor = 0;
            this.hpPenalty = 0;
        }

        @Override
        public String toString() {
            return "State [hp=" + hp + ", mana=" + mana + ", manaSpent="
                    + manaSpent + ", bossHp=" + bossHp + ", bossDamage="
                    + bossDamage + ", armor=" + armor + ", hpPenalty="
                    + hpPenalty + ", activeEffects=" + activeEffects + "]";
        }

        public State copy() {
            return new State(hp, mana, manaSpent, bossHp, bossDamage, armor,
                    hpPenalty, activeEffects);
        }

        private boolean validSpell(Spell spell) {
            return !activeEffects.containsKey(spell) && spell.cost <= mana;
        }

        private Collection<Spell> validSpells() {
            return Arrays.stream(Spell.values()).filter(s -> validSpell(s))
                    .collect(Collectors.toList());
        }

        private static State applyEffectsTo(State orig) {
            State newstate = orig.copy();

            newstate.armor = 0;

            List<Spell> effects = new ArrayList<>();
            effects.addAll(newstate.activeEffects.keySet());

            for (Spell spell : effects) {
                spell.applyEffect(newstate);

                int timer = newstate.activeEffects.get(spell);
                if (timer <= 0) {
                    newstate.activeEffects.remove(spell);
                } else {
                    newstate.activeEffects.put(spell, timer - 1);
                }
            }

            return newstate;
        }

        private static State applySpellTo(State state, Spell spell) {
            State newstate = state.copy();
            spell.applySpell(newstate);
            newstate.mana -= spell.cost;
            newstate.manaSpent += spell.cost;
            return newstate;
        }

        public void applyPart2Penalty() {
            hp -= hpPenalty;
        }
    }

    int battlePlayer(State state, int currentBest) {
        state.applyPart2Penalty();

        if (state.hp <= 0) {
            return -1;
        } else {
            State s0 = State.applyEffectsTo(state);

            for (Spell spell : state.validSpells()) {
                State s1 = State.applySpellTo(s0, spell);

                if (s1.manaSpent > currentBest) {
                    continue;
                }

                int bestMana = battleBoss(s1, currentBest);
                if (bestMana == -1)
                    continue;

                currentBest = Math.min(currentBest, bestMana);
            }

            return currentBest;
        }
    }

    int battleBoss(State state, int currentBest) {
        if (state.bossHp <= 0) {
            if (state.manaSpent < currentBest)
                System.out.println("Boss dies, mana spent: " + state.manaSpent);
            return state.manaSpent;
        } else {
            State s0 = State.applyEffectsTo(state);
            s0.hp -= Math.max(1, s0.bossDamage - s0.armor);
            return battlePlayer(s0, currentBest);
        }
    }

    @Test
    public void testDay22() throws Exception {
        System.out.println("Part 1");
        State state = new State(50, 500);
        assertEquals(900, battlePlayer(state, Integer.MAX_VALUE));

        System.out.println("Part 2");
        state = new State(50, 500);
        state.hpPenalty = 1;
        assertEquals(1216, battlePlayer(state, Integer.MAX_VALUE));
    }
}
