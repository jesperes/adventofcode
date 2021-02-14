package aoc2018;

import java.io.File;
import java.io.IOException;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.TreeSet;
import java.util.stream.Collectors;

import com.google.common.collect.ComparisonChain;

import common2.AocBaseRunner;
import common2.AocPuzzleInfo;
import common2.AocResult;
import common2.IAocIntPuzzle;
import common2.InputUtils;

/**
 * The dreaded Elf vs. Goblins puzzle. This uses a single A* search per round,
 * searching backwards from all enemies towards the start square.
 */
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

    record Pos(int x, int y) implements Comparable<Pos> {
        @Override
        public int compareTo(Pos o) {
            return (y != o.y) ? Integer.compare(y, o.y)
                    : Integer.compare(x, o.x);
        }

        public Set<Pos> getNeighbors() {
            Set<Pos> set = new HashSet<>();
            set.add(new Pos(x, y - 1));
            set.add(new Pos(x - 1, y));
            set.add(new Pos(x + 1, y));
            set.add(new Pos(x, y + 1));
            return set;
        }
    }

    class Unit implements Comparable<Unit> {
        Pos pos;
        boolean isElf;
        int hp = 200;
        int attackPower = 3;

        public Unit(Pos pos, boolean isElf, int attackPower) {
            this.pos = pos;
            this.isElf = isElf;
            this.attackPower = attackPower;
        }

        @Override
        public int compareTo(Unit o) {
            return pos.compareTo(o.pos);
        }
    }

    public class Cavern {
        public Map<Pos, Character> map = new HashMap<>();
        public Map<Pos, Unit> units = new HashMap<>();
        int width = 0;
        int height = 0;
        public boolean allowElfDeath = true;

        public Cavern(String grid, int elfAP) {
            int goblinAP = 3; // goblin attack power is always 3
            int y = 0;
            for (String line : grid.split("\n")) {
                int x = 0;
                width = line.length();
                for (char c : line.trim().toCharArray()) {
                    Pos pos = new Pos(x, y);
                    // @formatter:off
                    switch (c) {
                    case '#': map.put(pos, c); break;
                    case 'G': units.put(pos, new Unit(pos, false, goblinAP)); break;
                    case 'E': units.put(pos, new Unit(pos, true, elfAP)); break;
                    case '.': break;
                    }
                    // @formatter:on
                    x++;
                }
                height++;
                y++;
            }
        }

        public boolean isOpen(Pos pos) {
            return !map.containsKey(pos)
                    && !(units.containsKey(pos) && units.get(pos).hp > 0);
        }

        public void move(Unit unit, Pos newPos) {
            units.remove(unit.pos);
            unit.pos = newPos;
            units.put(newPos, unit);
        }

        public List<Unit> getSortedUnits() {
            return units.entrySet().stream().filter(e -> e.getValue().hp > 0)
                    .map(e -> e.getValue()).sorted()
                    .collect(Collectors.toList());
        }

        public List<Unit> getEnemies(Unit unit) {
            return units.entrySet().stream().map(e -> e.getValue())
                    .filter(u -> u.hp > 0 && u.isElf != unit.isElf)
                    .collect(Collectors.toList());
        }

        public List<Unit> getAdjacentEnemies(Unit unit) {
            return units.entrySet().stream().map(e -> e.getValue())
                    .filter(u -> u.hp > 0 && u.isElf != unit.isElf
                            && minCost(u.pos, unit.pos) == 1)
                    .sorted().collect(Collectors.toList());
        }
    }

    record SearchNode(int cost, Pos enemyPos, Pos estimatedLastPos, Pos prevPos,
            Pos current) implements Comparable<SearchNode> {
        @Override
        public int compareTo(SearchNode o) {
            return ComparisonChain.start().compare(cost, o.cost)
                    .compare(enemyPos, o.enemyPos)
                    .compare(estimatedLastPos, o.estimatedLastPos)
                    .compare(prevPos, o.prevPos).compare(current, o.current)
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
        Cavern cavern = new Cavern(input, 3);
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
        int minHp = enemies.stream().mapToInt(e -> e.hp).min().getAsInt();
        Unit enemyToAttack = enemies.stream().filter(e -> e.hp == minHp)
                .findFirst().get();
        enemyToAttack.hp -= unit.attackPower;
    }

    private MoveData move(Unit unit, List<Unit> enemies, Cavern cavern) {
        TreeSet<SearchNode> nodes = new TreeSet<>();
        Set<Pos> visited = new HashSet<>();

        for (Unit enemy : enemies) {
            for (Pos nbr : enemy.pos.getNeighbors()) {
                nodes.add(new SearchNode(minCost(nbr, unit.pos), enemy.pos,
                        estimatedLastPos(enemy.pos, unit.pos), enemy.pos, nbr));
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
