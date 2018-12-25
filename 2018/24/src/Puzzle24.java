import static org.junit.Assert.assertEquals;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.junit.Test;

public class Puzzle24 {
    static Pattern pattern = Pattern.compile(
            "(\\d+) units each with (\\d+) hit points (\\((.*)\\) )?with an attack that does (\\d+) (\\w+) damage at initiative (\\d+)");

    enum Army {
        ImmuneSystem, Infection
    }

    enum DamageType {
        cold, bludgeoning, radiation, slashing, fire
    }

    static class ArmyGroup {
        Army army;
        String id;
        int units;
        int hp;
        int damage;
        DamageType damageType;
        int initiative;
        List<DamageType> immuneTo;
        List<DamageType> weakTo;

        public ArmyGroup(Army army, int id, int units, int hp, int damage,
                DamageType damageType, int initiative,
                List<DamageType> immuneTo, List<DamageType> weakTo) {
            super();
            this.army = army;
            this.id = army.toString() + "-" + id;
            this.units = units;
            this.hp = hp;
            this.damage = damage;
            this.damageType = damageType;
            this.initiative = initiative;
            this.immuneTo = immuneTo;
            this.weakTo = weakTo;
        }

        @Override
        public boolean equals(Object obj) {
            ArmyGroup o = (ArmyGroup) obj;
            return id.equals(o.id);
        }

        @Override
        public String toString() {
            return "ArmyGroup [army=" + army + ", id=" + id + ", units=" + units
                    + ", hp=" + hp + ", damage=" + damage + ", damageType="
                    + damageType + ", initiative=" + initiative + ", immuneTo="
                    + immuneTo + ", weakTo=" + weakTo + "]";
        }

        public int getEffectivePower() {
            return units * damage;
        }

        /**
         * Defines the order in which this group selects its targets among the
         * available enemy groups.
         */
        private class EnemyTargetOrder implements Comparator<ArmyGroup> {
            @Override
            public int compare(ArmyGroup eg1, ArmyGroup eg2) {
                int ad1 = attackDamageAgainst(eg1);
                int ad2 = attackDamageAgainst(eg2);

                /*
                 * Select target against which this group would deal the largest
                 * attack damage
                 */
                if (ad1 != ad2)
                    return Integer.compare(ad1, ad2) * -1; // largest first
                else {
                    /*
                     * Break ties on largest effective power.
                     */

                    int ep1 = eg1.getEffectivePower();
                    int ep2 = eg2.getEffectivePower();
                    if (ep1 != ep2)
                        return Integer.compare(ep1, ep2) * -1; // largest first
                    else {
                        return Integer.compare(eg1.initiative, eg2.initiative)
                                * -1;
                    }
                }
            }
        }

        public Optional<ArmyGroup> selectTarget(List<ArmyGroup> groups,
                Map<String, ArmyGroup> selections) {

            List<ArmyGroup> enemyTargetOrder = new ArrayList<>();
            for (ArmyGroup g : groups) {
                if (!g.army.equals(army) && !selections.containsValue(g))
                    enemyTargetOrder.add(g);
            }

            Collections.sort(enemyTargetOrder, new EnemyTargetOrder());
            // System.out.format("%s selects targets from %s%n", id,
            // enemyTargetOrder.stream().map(g -> g.id)
            // .collect(Collectors.toList()));

            return enemyTargetOrder.stream()
                    .filter(eg -> attackDamageAgainst(eg) > 0).findFirst();
        }

        public int attackDamageAgainst(ArmyGroup defender) {
            int ep = getEffectivePower();
            if (defender.immuneTo.contains(damageType)) {
                return 0;
            } else if (defender.weakTo.contains(damageType)) {
                return ep * 2;
            } else {
                return ep;
            }
        }
    }

    private static class BattleResult {
        long units;
        Army winner;

        public BattleResult(long units, Army winner) {
            super();
            this.units = units;
            this.winner = winner;
        }

        @Override
        public String toString() {
            return "BattleResult [units=" + units + ", winner=" + winner + "]";
        }

        @Override
        public boolean equals(Object obj) {
            BattleResult o = (BattleResult) obj;
            return units == o.units && winner.equals(o.winner);
        }

    }

    /**
     * Defines the order in which groups select their targets, i.e. which group
     * gets to pick first, second, etc.
     */
    private static class OrderOfSelection implements Comparator<ArmyGroup> {
        @Override
        public int compare(ArmyGroup o1, ArmyGroup o2) {
            /*
             * The group with highest effective power gets to choose first.
             * Break ties using group initiative.
             */
            int ep1 = o1.getEffectivePower();
            int ep2 = o2.getEffectivePower();

            if (ep1 != ep2) {
                return Integer.compare(ep1, ep2) * -1;
            } else {
                return Integer.compare(o1.initiative, o2.initiative) * -1;
            }
        }
    }

    /**
     * Defines the order in which groups perform their selected attacks.
     */
    private static class OrderOfAttack implements Comparator<ArmyGroup> {
        @Override
        public int compare(ArmyGroup o1, ArmyGroup o2) {
            /*
             * Units attack in decreasing order or initiative.
             */
            return Integer.compare(o1.initiative, o2.initiative) * -1;
        }
    }

    @Test
    public void testPart1() throws Exception {
        assertEquals(new BattleResult(5216L, Army.Infection),
                run("testinput.txt"));
        assertEquals(new BattleResult(25088L, Army.Infection),
                run("input.txt"));
    }

    @Test
    public void testPart2() throws Exception {
        assertEquals(51, findLowestBoost("testinput.txt"));
        assertEquals(0, findLowestBoost("input.txt"));
    }

    private static BattleResult run(String filename)
            throws FileNotFoundException, IOException {
        return run(filename, 0);
    }

    private static int numUnits(List<ArmyGroup> list, Army army) {
        return list.stream().filter(g -> g.army == army).mapToInt(g -> g.units)
                .sum();
    }

    private static BattleResult run(String filename, int boost)
            throws FileNotFoundException, IOException {
        List<ArmyGroup> armyGroups = parse(filename, boost);

        // int n = 1;
        while (true) {
            // System.out.format("%nAttack round %d commencing...%n", n++);

            // Sort the groups in selection order.
            Collections.sort(armyGroups, new OrderOfSelection());

            Map<String, ArmyGroup> targetSelections = new HashMap<>();

            for (ArmyGroup g : armyGroups) {
                g.selectTarget(armyGroups, targetSelections).ifPresent(
                        target -> targetSelections.put(g.id, target));
            }

            // Resort the list of groups in attack order.
            Collections.sort(armyGroups, new OrderOfAttack());

            // System.out.println(">>> Target selections:");
            // for (Entry<String, ArmyGroup> entry :
            // targetSelections.entrySet()) {
            // System.out.format("%s attacks %s%n", entry.getKey(),
            // entry.getValue().id);
            // }
            // System.out.println("<<<");
            long immuneSystemUnitsPre = numUnits(armyGroups, Army.ImmuneSystem);
            long infectionUnitsPre = numUnits(armyGroups, Army.Infection);

            for (ArmyGroup attacker : armyGroups) {
                // If the target selections does not contain a key for this
                // group, then it did not select a target.
                if (targetSelections.containsKey(attacker.id)) {

                    ArmyGroup defender = targetSelections.get(attacker.id);

                    int ad = attacker.attackDamageAgainst(defender);
                    int killedUnits = ad / defender.hp;
                    if (killedUnits > defender.units)
                        killedUnits = defender.units;
                    defender.units -= killedUnits;

                    // System.out.format("%s attacked %s, killing %d units%n",
                    // attacker.id, defender.id, killedUnits);
                }
            }

            armyGroups.removeIf(ag -> ag.units <= 0);

            // for (ArmyGroup group : armyGroups) {
            // System.out.format("Group %s, units left %d%n", group.id,
            // group.units);
            // }

            long immuneSystemUnits = numUnits(armyGroups, Army.ImmuneSystem);
            long infectionUnits = numUnits(armyGroups, Army.Infection);

            if (immuneSystemUnits == 0) {
                return new BattleResult(infectionUnits, Army.Infection);
            } else if (infectionUnits == 0) {
                return new BattleResult(immuneSystemUnits, Army.ImmuneSystem);
            }

            if (immuneSystemUnits == immuneSystemUnitsPre
                    && infectionUnits == infectionUnitsPre) {
                System.out.println("Stalemate!");
                if (immuneSystemUnits > infectionUnits) {
                    return new BattleResult(immuneSystemUnits,
                            Army.ImmuneSystem);
                } else {
                    return new BattleResult(infectionUnits, Army.Infection);
                }
            }
        }
    }

    private static List<ArmyGroup> parse(String filename, int boost)
            throws FileNotFoundException, IOException {
        File inputfile = new File(filename);
        List<ArmyGroup> groups = new ArrayList<>();

        try (BufferedReader reader = new BufferedReader(
                new FileReader(inputfile))) {
            String line = null;
            Army army = null;
            int currid = 0;
            while ((line = reader.readLine()) != null) {
                if (line.contains("Immune System:")) {
                    army = Army.ImmuneSystem;
                    currid = 1;
                } else if (line.contains("Infection")) {
                    army = Army.Infection;
                    currid = 1;
                } else {
                    Matcher m = pattern.matcher(line);
                    if (m.matches()) {
                        int units = Integer.valueOf(m.group(1));
                        int hp = Integer.valueOf(m.group(2));
                        int damage = Integer.valueOf(m.group(5));
                        if (army == Army.ImmuneSystem)
                            damage += boost;
                        DamageType damageType = DamageType.valueOf(m.group(6));
                        int initiative = Integer.valueOf(m.group(7));

                        List<DamageType> weakTo = new ArrayList<>();
                        List<DamageType> immuneTo = new ArrayList<>();

                        String weaknesses = m.group(4);
                        if (weaknesses != null) {
                            for (String s : weaknesses.split(";")) {
                                String s0 = s.trim();
                                if (s0.startsWith("weak to")) {
                                    for (String weakness : s0
                                            .substring("weak to ".length())
                                            .split(",")) {
                                        weakTo.add(DamageType
                                                .valueOf(weakness.trim()));
                                    }
                                }
                                if (s0.startsWith("immune to")) {
                                    for (String immunity : s0
                                            .substring("immune to ".length())
                                            .split(",")) {
                                        immuneTo.add(DamageType
                                                .valueOf(immunity.trim()));
                                    }
                                }
                            }
                        }
                        groups.add(new ArmyGroup(army, currid++, units, hp,
                                damage, damageType, initiative, immuneTo,
                                weakTo));
                    }
                }
            }
        }

        return groups;
    }

    private static long findLowestBoost(String filename)
            throws FileNotFoundException, IOException {
        int boost = 0;
        while (boost < 2000) {
            BattleResult result = run(filename, boost);
            if (result.winner.equals(Army.ImmuneSystem)) {
                System.out.format(
                        "Immune system won with %d units (boost %d)%n",
                        result.units, boost);
                return result.units;
            } else {
                System.out.format("Boost = %d, result = %s%n", boost, result);
                boost++;
            }
        }
        return -1;
    }
}
