package aoc2018;

import java.io.File;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import common2.AocBaseRunner;
import common2.AocPuzzleInfo;
import common2.AocResult;
import common2.IAocLongPuzzle;
import common2.InputUtils;

public class Day24 implements IAocLongPuzzle<List<String>> {

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
                        // If all else fails, tie break on initiative, largest
                        // first
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
        long winnerUnits;
        long loserUnits;
        Army winner;

        public BattleResult(long winnerUnits, long loserUnits, Army winner) {
            super();
            this.winnerUnits = winnerUnits;
            this.loserUnits = loserUnits;
            this.winner = winner;
        }

        @Override
        public String toString() {
            return "BattleResult [winnerUnits=" + winnerUnits + ", loserUnits="
                    + loserUnits + ", winner=" + winner + "]";
        }

        @Override
        public boolean equals(Object obj) {
            BattleResult o = (BattleResult) obj;
            return winnerUnits == o.winnerUnits && loserUnits == o.loserUnits
                    && winner.equals(o.winner);
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

    private static int numUnits(List<ArmyGroup> list, Army army) {
        return list.stream().filter(g -> g.army == army).mapToInt(g -> g.units)
                .sum();
    }

    private BattleResult run(List<String> lines, int boost) {
        List<ArmyGroup> armyGroups = parse(lines, boost);

        int stalemateAttempts = 5;

        while (true) {
            // Sort the groups in selection order.
            Collections.sort(armyGroups, new OrderOfSelection());

            Map<String, ArmyGroup> targetSelections = new HashMap<>();

            for (ArmyGroup g : armyGroups) {
                g.selectTarget(armyGroups, targetSelections).ifPresent(
                        target -> targetSelections.put(g.id, target));
            }

            // Resort the list of groups in attack order.
            Collections.sort(armyGroups, new OrderOfAttack());

            boolean anyKilled = false;

            for (ArmyGroup attacker : armyGroups) {
                // If the target selections does not contain a key for this
                // group, then it did not select a target.
                if (targetSelections.containsKey(attacker.id)) {

                    ArmyGroup defender = targetSelections.get(attacker.id);

                    int ad = attacker.attackDamageAgainst(defender);
                    int killedUnits = ad / defender.hp;
                    if (killedUnits > defender.units)
                        killedUnits = defender.units;

                    if (killedUnits > 0)
                        anyKilled = true;

                    defender.units -= killedUnits;

                }
            }

            armyGroups.removeIf(ag -> ag.units <= 0);

            long immuneSystemUnits = numUnits(armyGroups, Army.ImmuneSystem);
            long infectionUnits = numUnits(armyGroups, Army.Infection);

            if (immuneSystemUnits == 0) {
                return new BattleResult(infectionUnits, immuneSystemUnits,
                        Army.Infection);
            } else if (infectionUnits == 0) {
                return new BattleResult(immuneSystemUnits, infectionUnits,
                        Army.ImmuneSystem);
            }

            if (!anyKilled) {
                stalemateAttempts--;
            }

            /*
             * Always treat stalemates as infection victories.
             */
            if (stalemateAttempts < 0) {
                return new BattleResult(infectionUnits, immuneSystemUnits,
                        Army.Infection);
            }
        }
    }

    @Override
    public AocPuzzleInfo getInfo() {
        return new AocPuzzleInfo(2018, 24, "Immune System Simulator 20XX",
                true);
    }

    @Override
    public AocResult<Long, Long> getExpected() {
        return AocResult.of(25088L, 2002L);
    }

    private List<ArmyGroup> parse(List<String> lines, int boost) {
        List<ArmyGroup> groups = new ArrayList<>();

        Army army = null;
        int currid = 0;
        for (String line : lines) {
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
                    groups.add(new ArmyGroup(army, currid++, units, hp, damage,
                            damageType, initiative, immuneTo, weakTo));
                }
            }
        }
        return groups;
    }

    @Override
    public List<String> parse(Optional<File> file) {
        return InputUtils.asStringList(file.get());
    }

    @Override
    public Long part1(List<String> input) {
        return run(input, 0).winnerUnits;
    }

    @Override
    public Long part2(List<String> input) {
        return findLowestBoost(input);
    }

    private long findLowestBoost(List<String> lines) {
        int boost = 0;
        while (boost < 2000) {
            BattleResult result = run(lines, boost);
            if (result.winner.equals(Army.ImmuneSystem)) {
                return result.winnerUnits;
            } else {
                boost++;
            }
        }
        return -1;
    }

    public static void main(String[] args) {
        AocBaseRunner.run(new Day24());
    }
}
