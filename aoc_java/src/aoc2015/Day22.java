package aoc2015;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;

import aoc2015.Day22.Input;
import common2.AocBaseRunner;
import common2.AocPuzzleInfo;
import common2.AocResult;
import common2.IAocIntPuzzle;

public class Day22 implements IAocIntPuzzle<Input> {

    record Input(int hp, int damage) {
    }

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

        public State(Input input, int hp, int mana, int hpPenalty) {
            this.hp = hp;
            this.mana = mana;
            this.manaSpent = 0;
            this.bossHp = input.hp;
            this.bossDamage = input.damage;
            this.armor = 0;
            this.hpPenalty = hpPenalty;
        }

        public State copy() {
            return new State(hp, mana, manaSpent, bossHp, bossDamage, armor,
                    hpPenalty, activeEffects);
        }

        private Collection<Spell> validSpells() {
            List<Spell> spells = new ArrayList<>();
            for (Spell s : Spell.values()) {
                if (!activeEffects.containsKey(s) && s.cost <= mana)
                    spells.add(s);
            }
            return spells;
        }

        private static State applyEffectsTo(State orig) {
            State newstate = orig.copy();

            newstate.armor = 0;

            List<Spell> effects = new ArrayList<>();
            effects.addAll(newstate.activeEffects.keySet());

            for (Spell spell : effects) {
                spell.applyEffect(newstate);

                int timer = newstate.activeEffects.get(spell);
                if (timer <= 1) {
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

    int battlePlayer(State state) {
        return battlePlayer(state, Integer.MAX_VALUE);
    }

    int battlePlayer(State state, int currentBest) {
        state.applyPart2Penalty();

        if (state.hp <= 0) {
            return -1;
        } else {
            State s0 = State.applyEffectsTo(state);

            for (Spell spell : s0.validSpells()) {
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
            return state.manaSpent;
        } else {
            State s0 = State.applyEffectsTo(state);
            if (s0.bossHp <= 0) {
                return s0.manaSpent;
            }
            s0.hp -= Math.max(1, s0.bossDamage - s0.armor);
            return battlePlayer(s0, currentBest);
        }
    }

    @Override
    public AocPuzzleInfo getInfo() {
        return new AocPuzzleInfo(2015, 22, "Wizard Simulator 20XX", false);
    }

    @Override
    public AocResult<Integer, Integer> getExpected() {
        return AocResult.of(900, 1216);
    }

    @Override
    public Input parse(Optional<File> file) throws IOException {
        return new Input(51, 9);
    }

    @Override
    public Integer part1(Input input) {
        return battlePlayer(new State(input, 50, 500, 0));
    }

    @Override
    public Integer part2(Input input) {
        return battlePlayer(new State(input, 50, 500, 1));
    }

    public static void main(String[] args) {
        AocBaseRunner.run(new Day22());
    }
}
