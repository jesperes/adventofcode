package aoc2018;

import static org.junit.Assert.assertTrue;

import java.io.File;
import java.io.IOException;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.TreeSet;
import java.util.stream.Collectors;

import com.google.common.collect.ComparisonChain;

import aoc2018.day15.Cavern;
import aoc2018.day15.Pos;
import aoc2018.day15.Unit;
import common2.AocBaseRunner;
import common2.AocPuzzleInfo;
import common2.AocResult;
import common2.IAocIntPuzzle;
import common2.InputUtils;

public class Day15 implements IAocIntPuzzle<String> {

    enum Outcome {
        InProgress, NoEnemies, ElfDeath
    }

    record CombatResult(Outcome outcome, int result) {
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
        return doCombat(cavern).result;
    }

    @Override
    public Integer part2(String input) {
        for (int ap = 4;; ap++) {
            Cavern cavern = new Cavern(input, ap);
            cavern.allowElfDeath = false;

            CombatResult result = doCombat(cavern);
            if (result.outcome == Outcome.ElfDeath)
                continue;
            else
                return result.result;
        }
    }

    // Do combat rounds until one side wins.
    CombatResult doCombat(Cavern cavern) {
        int rounds = 0;
        while (true) {
            Outcome outcome = doRound(cavern);
            switch (outcome) {
            case InProgress:
                rounds++;
                continue;
            case NoEnemies:
                return new CombatResult(outcome, cavern.units.values().stream()
                        .mapToInt(unit -> unit.hp).sum() * rounds);
            case ElfDeath:
                return new CombatResult(outcome, 0);
            }
        }
    }

    // Execute one round (every unit takes its turn)
    Outcome doRound(Cavern cavern) {
        Outcome result = Outcome.InProgress;

        for (Unit unit : cavern.getSortedUnits()) {
            if (unit.hp <= 0)
                continue;

            List<Unit> enemies = cavern.getEnemies(unit);
            if (enemies.size() == 0) {
                result = Outcome.NoEnemies;
                break;
            } else {
                MoveData move = move(unit, enemies, cavern);
                switch (move.result) {
                case Move:
                    cavern.move(unit, move.pos);
                    break;
                case Attack:
                    attack(cavern, unit);
                    break;
                case MoveAndAttack:
                    cavern.move(unit, move.pos);
                    attack(cavern, unit);
                    break;
                case NoRoute:
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
                return Outcome.ElfDeath;
            }
        }

        return result;
    }

    private void attack(Cavern cavern, Unit unit) {
        List<Unit> enemies = cavern.getAdjacentEnemies(unit);
        assertTrue(enemies.size() > 0);
        Collections.sort(enemies);
        int minHp = enemies.stream().mapToInt(e -> e.hp).min().getAsInt();
        Unit enemyToAttack = enemies.stream().filter(e -> e.hp == minHp)
                .findFirst().get();
        enemyToAttack.hp -= unit.attackPower;
    }

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

        for (Unit enemy : enemies) {
            for (Pos nbr : enemy.pos.getNeighbors()) {
                nodes.add(new SearchNode(//
                        minCost(nbr, unit.pos), // heuristic + route cost
                        enemy.pos, // enemy position
                        estimatedLastPos(enemy.pos, unit.pos), //
                        enemy.pos, //
                        nbr)); // current node
            }
        }

        while (true) {
            SearchNode node = nodes.pollFirst();

            if (node == null)
                return new MoveData(MoveResult.NoRoute, null);

            if (node.cost == 0 && node.current.equals(unit.pos)) {
                return new MoveData(MoveResult.Attack, null);
            } else if (node.cost == 1 && node.current.equals(unit.pos)) {
                return new MoveData(MoveResult.MoveAndAttack, node.prevPos);
            } else if (node.current.equals(unit.pos)) {
                return new MoveData(MoveResult.Move, node.prevPos);
            } else if (!visited.contains(node.current)
                    && cavern.isOpen(node.current)) {
                visited.add(node.current);
                int c = node.cost - minCost(node.current, unit.pos) + 1;
                for (Pos nbr : node.current.getNeighbors()) {
                    var n = new SearchNode(c + minCost(nbr, unit.pos),
                            node.enemyPos, estimatedLastPos(nbr, unit.pos),
                            node.current, nbr);
                    nodes.add(n);
                }
            }
        }
    }

    int minCost(Pos a, Pos b) {
        return Math.abs(a.x() - b.x()) + Math.abs(a.y() - b.y());
    }

    Pos estimatedLastPos(Pos source, Pos target) {
        if (source.y() < target.y()) {
            return new Pos(target.x(), target.y() - 1);
        } else if (source.x() < target.x()) {
            return new Pos(target.x() - 1, target.y());
        } else if (source.x() > target.x()) {
            return new Pos(target.x() + 1, target.y());
        } else {
            return new Pos(target.x(), target.y() + 1);
        }
    }

    public static void main(String[] args) {
        AocBaseRunner.run(new Day15());
    }
}
