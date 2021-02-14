package aoc2018;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashSet;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.TreeSet;
import java.util.stream.Collectors;

import org.junit.Test;

import com.google.common.collect.ComparisonChain;
import com.google.common.primitives.Ints;

import aoc2018.day15.Cavern;
import aoc2018.day15.Pos;
import aoc2018.day15.Unit;
import common2.AocBaseRunner;
import common2.AocPuzzleInfo;
import common2.AocResult;
import common2.AocTrace;
import common2.IAocIntPuzzle;
import common2.InputUtils;

public class Day15b implements IAocIntPuzzle<String> {

    @Override
    public AocPuzzleInfo getInfo() {
        return new AocPuzzleInfo(2018, 15, "Beverage Bandits", true);
    }

    @Override
    public AocResult<Integer, Integer> getExpected() {
        return AocResult.of(237996, 69700);
    }

    @Override
    public String parse(Optional<File> file) throws IOException {
        return InputUtils.asString(file.get());
    }

    @Override
    public Integer part1(String input) {
        Cavern cavern = new Cavern(input);

        cavern.allowElfDeath = true;
        int rounds = 0;

        AocTrace.trace(this, "PART1%n");

        while (true) {
            AocTrace.trace(this, "Round %d%n", rounds + 1);
            var result = doRound(cavern);
            AocTrace.trace(this, cavern.toString() + "\n---\n");
            switch (result) {
            case CombatInProgress:
                rounds++;
                continue;
            case NoEnemies:
                int sum = cavern.units.values().stream()
                        .mapToInt(unit -> unit.hp).sum();
                AocTrace.trace(this, "Round finished, %d * %d = %d%n", rounds,
                        sum, rounds * sum);
                return sum * rounds;
            case ElfDeath:
                throw new RuntimeException();
            }
        }
    }

    @Override
    public Integer part2(String input) {
        int elfAP = 4;
        while (true) {
            Cavern cavern = new Cavern(input, elfAP);
            cavern.allowElfDeath = false;
            int rounds = 0;
            AocTrace.trace(this, "PART2, elf attack power %d%n", elfAP);

            boolean hasElfDied = false;

            while (!hasElfDied) {
                AocTrace.trace(this, "Round %d%n", rounds + 1);
                RoundResult result = doRound(cavern);
                AocTrace.trace(this, cavern.toString() + "\n---\n");
                switch (result) {
                case CombatInProgress:
                    rounds++;
                    continue;
                case NoEnemies:
                    AocTrace.trace(this, "PART2, success at elfAP %d%n", elfAP);
                    int sum = cavern.units.values().stream()
                            .mapToInt(unit -> unit.hp).sum();
                    return sum * rounds;
                case ElfDeath:
                    System.out.println("Elf death. Increasing attack power to "
                            + (elfAP + 1));
                    elfAP++;
                    hasElfDied = true;
                    break;
                }
            }
        }
    }

    enum RoundResult {
        CombatInProgress, NoEnemies, ElfDeath
    }

    enum MoveResult {
        NoRoute, Attack, Move, MoveAndAttack
    }

    record MoveData(MoveResult result, Pos pos) {
    }

    record SearchNode(//
            int cost, // heuristic + route cost
            Pos enemyPos, // enemy position
            Pos estimatedLastPos, //
            Pos prevPos, // previous position on the route
            Pos current) // current position
            implements Comparable<SearchNode> {
        @Override
        public int compareTo(SearchNode o) {
            return ComparisonChain.start() //
                    .compare(cost, o.cost) //
                    .compare(enemyPos, o.enemyPos) //
                    .compare(estimatedLastPos, o.estimatedLastPos) //
                    .compare(prevPos, o.prevPos) //
                    .compare(current, o.current) //
                    .result();
        }
    }

    void traceMove(Unit unit, Pos newPos) {
        AocTrace.trace(this,
                String.format("%c unit at {%d,%d} moved to {%d,%d}%n",
                        unit.isElf ? 'E' : 'G', unit.pos.y(), unit.pos.x(),
                        newPos.y(), newPos.x()));
    }

    RoundResult doRound(Cavern cavern) {
        boolean noEnemies = false;

        for (Unit unit : cavern.getSortedUnits()) {
            if (unit.hp <= 0)
                continue;

            List<Unit> enemies = cavern.getEnemies(unit);
            if (enemies.size() == 0) {
                noEnemies = true;
                break;
            } else {
                // System.out.println(cavern);
                MoveData move = move(unit, enemies, cavern);
                switch (move.result) {
                case Move:
                    traceMove(unit, move.pos);
                    cavern.move(unit, move.pos);
                    break;
                case Attack:
                    attack(cavern, unit);
                    break;
                case MoveAndAttack:
                    traceMove(unit, move.pos);
                    cavern.move(unit, move.pos);
                    attack(cavern, unit);
                    break;
                case NoRoute:
                    // unit cannot find any route to an enemy
                    break;
                default:
                    throw new RuntimeException();
                }
            }
        }

        List<Pos> deadUnits = cavern.units.entrySet().stream()
                .filter(e -> e.getValue().hp <= 0).map(e -> e.getKey())
                .collect(Collectors.toList());

        for (Pos pos : deadUnits) {
            Unit deadUnit = cavern.units.get(pos);
            cavern.units.remove(pos);
            if (deadUnit.isElf && !cavern.allowElfDeath) {
                return RoundResult.ElfDeath;
            }
        }

        return noEnemies ? RoundResult.NoEnemies : RoundResult.CombatInProgress;
    }

    private void attack(Cavern cavern, Unit unit) {
        List<Unit> enemies = cavern.getAdjacentEnemies(unit);
        assertTrue(enemies.size() > 0);
        Collections.sort(enemies);
        int minHp = enemies.stream().mapToInt(e -> e.hp).min().getAsInt();
        Unit enemyToAttack = enemies.stream().filter(e -> e.hp == minHp)
                .findFirst().get();
        enemyToAttack.hp -= unit.attackPower;
        if (enemyToAttack.hp <= 0) {
            traceKill(unit, enemyToAttack);
        } else {
            traceAttack(unit, enemyToAttack);
        }
    }

    void traceKill(Unit unit, Unit enemyUnit) {
        AocTrace.trace(this,
                "%c unit at {%d,%d} killed %c unit at {%d,%d}%n".formatted(
                        unit.isElf ? 'E' : 'G', unit.pos.y(), unit.pos.x(),
                        enemyUnit.isElf ? 'E' : 'G', enemyUnit.pos.y(),
                        enemyUnit.pos.x()));
    }

    void traceAttack(Unit unit, Unit enemyUnit) {
        AocTrace.trace(this,
                "%c unit at {%d,%d} attacked %c unit at {%d,%d}%n".formatted(
                        unit.isElf ? 'E' : 'G', unit.pos.y(), unit.pos.x(),
                        enemyUnit.isElf ? 'E' : 'G', enemyUnit.pos.y(),
                        enemyUnit.pos.x()));
    }

    Pos estimatedLastPos(Pos source, Collection<Pos> targetNeighbors) {
        return targetNeighbors.stream().sorted(new Comparator<Pos>() {
            @Override
            public int compare(Pos o1, Pos o2) {
                int d1 = minCost(o1, source);
                int d2 = minCost(o2, source);
                return Ints.compare(d1, d2);
            }
        }).findFirst().get();
    }

    List<Pos> x = new ArrayList<>();

    /**
     * Do a reverse A*-search from all the enemies backwards to the unit which
     * is about to move. The priorities of the puzzle is encoded in the way we
     * compare nodes, see {@link SearchNode#compareTo(SearchNode)}.
     * 
     * This allows us to do only a single A* search per unit per move.
     */
    private MoveData move(Unit unit, List<Unit> enemies, Cavern cavern) {
        TreeSet<SearchNode> nodes = new TreeSet<>();
        Set<Pos> visited = new HashSet<>();
        Collection<Pos> targetNeighbors = unit.pos.getNeighbors();

        for (Unit enemy : enemies) {
            for (Pos nbr : enemy.pos.getNeighbors()) {
                nodes.add(new SearchNode(//
                        minCost(nbr, unit.pos), // heuristic + route cost
                        enemy.pos, // enemy position
                        estimatedLastPos(enemy.pos, targetNeighbors), //
                        enemy.pos, //
                        nbr)); // current node
            }
        }

        while (true) {
            SearchNode node = nodes.pollFirst();

            if (node == null)
                return new MoveData(MoveResult.NoRoute, null);
            else {
                if (node.cost == 0 && node.current.equals(unit.pos)) {
                    // The unit is directly adjacent to an enemy position,
                    // we can attack directly. (This will only happen
                    // when node is one of the starting nodes.)
                    return new MoveData(MoveResult.Attack, null);
                } else if (node.cost == 1 && node.current.equals(unit.pos)) {
                    // The unit can move to "prevPos" and then attack.
                    return new MoveData(MoveResult.MoveAndAttack, node.prevPos);
                } else if (node.current.equals(unit.pos)) {
                    // We have found the shortest path towards the enemy, but
                    // we cannot attack yet.
                    // System.out.println(node);
                    return new MoveData(MoveResult.Move, node.prevPos);
                } else if (!visited.contains(node.current)
                        && cavern.isOpen(node.current)) {

                    visited.add(node.current);

                    // Cost to this position
                    int c = node.cost - minCost(node.current, unit.pos) + 1;

//                    if (node.current.y() == 23
//                            && node.enemyPos.equals(new Pos(14, 25)))
//                        System.out.format("At node %s%n", node);

                    for (Pos nbr : node.current.getNeighbors()) {
                        var n = new SearchNode(c + minCost(nbr, unit.pos),
                                node.enemyPos,
                                estimatedLastPos(nbr, targetNeighbors),
                                node.current, nbr);

                        //
//                        if (node.current.y() == 23
//                                && node.enemyPos.equals(new Pos(14, 25))
//                                && !cavern.units.containsKey(n.current)) {
//                            System.out.format(" --> %s%n", n);
//                            x.add(n.current);
//                        }
                        //
                        nodes.add(n);
                    }

//                    List<Pos> frontier = new ArrayList<>();
//                    for (var n : nodes) {
//                        if (n.enemyPos.equals(new Pos(14, 25))
//                                && cavern.isOpen(n.current)) {
//                            System.out.println(n);
//                            frontier.add(n.current);
//                        }
//                    }
//                    System.out.println("Best node: " + node);
////                    System.out.println("Frontier:\n"
////                            + cavern.toString(frontier, '@', node.current));
//                    System.out.println(cavern);
//                    System.out.println();
//
                }
            }
        }
    }

    int minCost(Pos a, Pos b) {
        return Math.abs(a.x() - b.x()) + Math.abs(a.y() - b.y());
    }

    @Test
    public void testEx1() throws Exception {
        String input = """
                #######
                #.G...#
                #...EG#
                #.#.#G#
                #..G#E#
                #.....#
                #######
                """;

        assertEquals(27730, (int) part1(input));
    }

    @Test
    public void testEx2() throws Exception {
        String input = """
                #######
                #G..#E#
                #E#E.E#
                #G.##.#
                #...#E#
                #...E.#
                #######
                """;

        assertEquals(36334, (int) part1(input));
    }

    @Test
    public void testEx3() throws Exception {
        String input = """
                #######
                #E..EG#
                #.#G.E#
                #E.##E#
                #G..#.#
                #..E#.#
                #######
                """;

        assertEquals(39514, (int) part1(input));
    }

    @Test
    public void testEx4() throws Exception {
        String input = """
                #######
                #E.G#.#
                #.#G..#
                #G.#.G#
                #G..#.#
                #...E.#
                #######
                """;

        assertEquals(27755, (int) part1(input));
    }

    @Test
    public void testEx5() throws Exception {
        String input = """
                #########
                #G......#
                #.E.#...#
                #..##..G#
                #...##..#
                #...#...#
                #.G...G.#
                #.....G.#
                #########
                """;

        assertEquals(18740, (int) part1(input));
    }

    @Test
    public void test6() throws Exception {
        String input = """
                ################################
                #############....###############
                #########.#......###############
                #########........###############
                ###########...##.###############
                ############..#...##############
                #############......#############
                ##########..#...#...############
                ########.............###########
                #######.................########
                ######..................########
                ######................##########
                #######.......#####...##########
                #######..G...#######..##########
                ########....#########.##########
                #.#.####..G.#########G##########
                #....#......#########E##..######
                #....#......#########.......####
                #...........#########.......####
                #.##.........#######........####
                #####.....G..G#####............#
                ######...G..GE...........#.....#
                ######..GG...GE..........#.#####
                ######.......GE.........##...###
                ########..G.GE..........#...####
                ########...G.GE...........######
                #######.....G..#.###############
                #########.....##################
                #########.....##################
                #########.....##################
                #########.....##################
                ################################
                """;

//        0 ################################ 
//        1 #############....############### 
//        2 #########.#......############### 
//        3 #########........############### 
//        4 ###########...##.############### 
//        5 ############..#...############## 
//        6 #############......############# 
//        7 ##########..#...#...############ 
//        8 ########.............########### 
//        9 #######.................######## 
//       10 ######..................######## 
//       11 ######................########## 
//       12 #######.......#####...########## 
//       13 #######..G...#######..########## {G,{13,9},200hp}
//       14 ########....#########.########## 
//       15 #.#.####..G.#########G########## {G,{15,10},200hp}, {G,{15,21},200hp}
//       16 #....#......#########E##..###### {E,{16,21},200hp}
//       17 #....#......#########.......#### 
//       18 #...........#########.......#### 
//       19 #.##.........#######........#### 
//       20 #####.....G..G#####............# {G,{20,10},200hp}, {G,{20,13},200hp}
//       21 ######...G..GE...........#.....# {G,{21,9},200hp}, {G,{21,12},200hp}, {E,{21,13},200hp}
//       22 ######..GG...GE..........#.##### {G,{22,8},200hp}, {G,{22,9},200hp}, {G,{22,13},200hp}, {E,{22,14},200hp}
//       23 ######.......GE.........##...### {G,{23,13},200hp}, {E,{23,14},200hp}
//       24 ########..G.GE..........#...#### {G,{24,10},200hp}, {G,{24,12},200hp}, {E,{24,13},200hp}
//       25 ########...G.GE...........###### {G,{25,11},200hp}, {G,{25,13},200hp}, {E,{25,14},200hp}
//       26 #######.....G..#.############### {G,{26,12},200hp}
//       27 #########.....################## 
//       28 #########.....################## 
//       29 #########.....################## 
//       30 #########.....################## 
//       31 ################################ 

        Cavern cavern = new Cavern(input);
        Unit unit = cavern.units.get(new Pos(9, 13));
        List<Unit> enemies = cavern.getEnemies(unit);
        MoveData move = move(unit, enemies, cavern);
        System.out.println(cavern.toString(x, '@', null));
        System.out.println(move);
    }

    public static void main(String[] args) {
        AocBaseRunner.run(new Day15b());
    }
}
