package aoc2015;

import static org.junit.Assert.assertEquals;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Optional;

import org.junit.Test;

import common2.AocBaseRunner;
import common2.AocPuzzleInfo;
import common2.AocResult;
import common2.IAocIntPuzzle;

public class Day21 implements IAocIntPuzzle<Void> {

    final static int bossHp = 100;
    final static int bossDamage = 8;
    final static int bossArmor = 2;
    final static int playerHp = 100;

    enum Item {
        None(0, 0, 0), //

        Dagger(8, 4, 0), //
        Shortsword(10, 5, 0), //
        Warhammer(25, 6, 0), //
        Longsword(40, 7, 0), //
        Greataxe(74, 8, 0), //

        Leather(13, 0, 1), //
        Chainmail(31, 0, 2), //
        Splintmail(53, 0, 3), //
        Bandedmail(75, 0, 4), //
        Platemail(102, 0, 5), //

        Damage1(25, 1, 0), //
        Damage2(50, 2, 0), //
        Damage3(100, 3, 0), //
        Defense1(20, 0, 1), //
        Defense2(40, 0, 2), //
        Defense3(80, 0, 3);

        int cost;
        int damage;
        int armor;

        Item(int cost, int damage, int armor) {
            this.cost = cost;
            this.damage = damage;
            this.armor = armor;
        }

    }

    final static Collection<Item> WEAPONS = Arrays.asList(Item.Dagger,
            Item.Shortsword, Item.Warhammer, Item.Longsword, Item.Greataxe);
    final static Collection<Item> ARMOR = Arrays.asList(Item.Leather,
            Item.Chainmail, Item.Splintmail, Item.Bandedmail, Item.Platemail,
            Item.None);
    final static Collection<Item> RINGS = Arrays.asList(Item.Damage1,
            Item.Damage2, Item.Damage3, Item.Defense1, Item.Defense2,
            Item.Defense3);

    static class Combatant {
        int hp;
        int damage;
        int armor;
        int cost;
        boolean isBoss;

        private Combatant(boolean isBoss, int hp, int damage, int armor,
                int cost) {
            this.hp = hp;
            this.damage = damage;
            this.armor = armor;
            this.isBoss = isBoss;
            this.cost = cost;
        }

        public static Combatant boss() {
            return new Combatant(true, bossHp, bossDamage, bossArmor, 0);
        }

        public static Combatant player(Item... items) {
            int cost = 0;
            int armor = 0;
            int damage = 0;

            for (Item item : items) {
                cost += item.cost;
                armor += item.armor;
                damage += item.damage;
            }

            return new Combatant(false, playerHp, damage, armor, cost);
        }

        void attack(Combatant defender) {
            defender.hp -= Math.max(1, damage - defender.armor);
        }
    }

    Combatant battle(Combatant attacker, Combatant defender) {
        attacker.attack(defender);
        if (defender.hp <= 0) {
            return attacker;
        } else {
            return battle(defender, attacker);
        }
    }

    List<Combatant> makeAllPlayers() {
        List<Combatant> players = new ArrayList<>();

        for (Item weapon : WEAPONS) {
            for (Item armor : ARMOR) {
                players.add(Combatant.player(weapon, armor));
                for (Item ring1 : RINGS) {
                    players.add(Combatant.player(weapon, armor, ring1));
                    for (Item ring2 : RINGS) {
                        players.add(
                                Combatant.player(weapon, armor, ring1, ring2));
                    }
                }
            }
        }
        return Collections.unmodifiableList(players);
    }

    @Test
    public void testDay21() throws Exception {

        int minGold = Integer.MAX_VALUE;
        int maxGold = Integer.MIN_VALUE;

        for (Combatant player : makeAllPlayers()) {
            Combatant boss = Combatant.boss();
            Combatant winner = battle(player, boss);

            if (!winner.isBoss && player.cost <= minGold) {
                minGold = player.cost;
            }
            if (winner.isBoss && player.cost >= maxGold) {
                maxGold = player.cost;
            }
        }

        assertEquals(91, minGold);
        assertEquals(158, maxGold);
    }

    @Override
    public AocPuzzleInfo getInfo() {
        return new AocPuzzleInfo(2015, 21, "RPG Simulator 20XX", false);
    }

    @Override
    public AocResult<Integer, Integer> getExpected() {
        return AocResult.of(91, 158);
    }

    @Override
    public Void parse(Optional<File> file) throws IOException {
        return null;
    }

    @Override
    public Integer part1(Void input) {
        int minGold = Integer.MAX_VALUE;

        for (Combatant player : makeAllPlayers()) {
            Combatant boss = Combatant.boss();
            Combatant winner = battle(player, boss);

            if (!winner.isBoss && player.cost <= minGold) {
                minGold = player.cost;
            }
        }

        return minGold;
    }

    @Override
    public Integer part2(Void input) {
        int maxGold = Integer.MIN_VALUE;

        for (Combatant player : makeAllPlayers()) {
            Combatant boss = Combatant.boss();
            Combatant winner = battle(player, boss);

            if (winner.isBoss && player.cost >= maxGold) {
                maxGold = player.cost;
            }
        }

        return maxGold;
    }

    public static void main(String[] args) {
        AocBaseRunner.run(new Day21());
    }
}
