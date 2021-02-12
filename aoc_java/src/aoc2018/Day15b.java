package aoc2018;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.TreeSet;
import java.util.stream.Collectors;

import org.junit.Test;

import aoc2018.day15.Cavern;
import aoc2018.day15.Node;
import aoc2018.day15.Pos;
import aoc2018.day15.Unit;
import common2.AocPuzzleInfo;
import common2.AocResult;
import common2.IAocIntPuzzle;
import common2.InputUtils;

/**
 * Combat proceeds in rounds. Each round, each unit still alive takes a turn.
 * 
 * 1. Moves into range (if it isn't already)
 * 
 * 1.1. Identify all possible targets (enemy units). If no targets remains,
 * combat ends.
 * 
 * 1.2. Identify all open squares adjacent to any target.
 * 
 * 1.3. Consider all such squares and determine which of those squares it can
 * reach in the fewest steps. (Break ties in reading order.)
 * 
 * 1.4. Take a single step towards that square along the shortest path to that
 * square. (Break ties in reading order.)
 * 
 * 2. Attacks an enemy (if it is in range)
 * 
 * 2.1. Determine all target that are in range. If no targets are in range, the
 * unit ends its turn.
 * 
 * 2.2. Select target with fewest hit points.
 * 
 * 2.3. Attack deals damage equal to the attack power of the attacker.
 * 
 */
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
        return 0;
    }

    @Override
    public Integer part2(String input) {
        return 0;
    }

    enum RoundResult {
        NoEnemies, ElfDeath
    }

    RoundResult doRound(Cavern cavern) {
        for (Unit unit : cavern.getSortedUnits()) {
            if (unit.hp <= 0)
                continue;

            List<Unit> enemies = cavern.getEnemies(unit);
            if (enemies.size() == 0)
                return RoundResult.NoEnemies;

            Set<Pos> inRange = cavern.getAdjacentTo(enemies);

            List<Node> nearest = findNearest(cavern, unit, inRange);
            System.out.println(nearest);
            System.out.println(cavern.toString(nearest.stream()
                    .map(node -> node.pos).collect(Collectors.toList()), '!'));

            Collections.sort(nearest);
            System.out
                    .println(cavern.toString(List.of(nearest.get(0).pos), '+'));

            System.out.println(
                    cavern.toString(nearest.stream().map(node -> node.start)
                            .collect(Collectors.toList()), '*'));

            return null;
        }
        return null;
    }

    private List<Node> findNearest(Cavern cavern, Unit unit, Set<Pos> inRange) {
        Set<Pos> startPositions = cavern.getAdjacentTo(List.of(unit));
        List<Node> nearest = new ArrayList<>();
        int dist = Integer.MAX_VALUE;

        for (Pos target : inRange) {

//            List<Node> startNodes = startPositions.stream()
//                    .map(start -> new Node(start, start))
//                    .collect(Collectors.toList());
//
//            a_star(startNodes, target);
//            List<Node> path = AStar.astar(starts, new IAStarCallbacks<Node>() {
//                @Override
//                public int heuristic(Node node) {
//                    return Math.abs(node.pos.x - target.x)
//                            + Math.abs(node.pos.y - target.y);
//                }
//
//                @Override
//                public Collection<Node> neighbors(Node node) {
//                    Set<Pos> set = new HashSet<>();
//                    cavern.getAdjacentTo(set, node.pos);
//                    return set.stream().map(pos -> new Node(node.start, pos))
//                            .collect(Collectors.toList());
//                }
//
//                @Override
//                public boolean isGoal(Node node) {
//                    return node.pos.equals(target);
//                }
//            });
//
            // Keep this path if it is closer or equally close to the
            // best one we have
//            int d = path.size();
//            if (d > 0) {
//                if (d > dist)
//                    continue;
//
//                if (d < dist) {
//                    nearest.clear();
//                    dist = d;
//                }
//                nearest.add(path.get(d - 1));
//            }
        }

        return nearest;
    }

    int manhattan(Pos a, Pos b) {
        return Math.abs(a.x - b.x) + Math.abs(a.y - b.y);
    }

    public List<Node> a_star(Cavern cavern, Collection<Node> startNodes,
            Pos target) {
        TreeSet<Node> openSet = new TreeSet<>();
        int bestPathLen = Integer.MAX_VALUE;
        List<Node> result = new ArrayList<>();

        for (Node start : startNodes) {
            openSet.add(start);
        }

        while (!openSet.isEmpty()) {
            var current = openSet.first();

            if (current.pos.equals(target)) {
                if (current.stepsFromStart <= bestPathLen) {
                    result.add(current);
                    bestPathLen = current.stepsFromStart;
                    System.out.println("Fount path of len %d: %s%n"
                            .formatted(bestPathLen, current));
                } else if (current.stepsFromStart > bestPathLen) {
                    System.out.println("No more paths");

                    return result;
                }
            }

            openSet.remove(current);

            // Get list of neighbors
            Set<Pos> set = new HashSet<>();
            cavern.getAdjacentTo(set, current.pos);
            List<Node> nbrs = set.stream()
                    .map(pos -> new Node(current.start, pos,
                            current.stepsFromStart + 1, manhattan(pos, target)))
                    .collect(Collectors.toList());

            for (Node nbr : nbrs) {
                if (!openSet.contains(nbr)) {
                    openSet.add(nbr);
                }
            }
        }

        return Collections.emptyList();
    }

    @Test
    public void test1() throws Exception {
        String input = """
                #######
                #E..G.#
                #...#.#
                #.G.#G#
                #######
                """;
        Cavern cavern = new Cavern(input);
        doRound(cavern);
    }

    @Test
    public void test2() throws Exception {
        String input = """
                #######
                #.E...#
                #.....#
                #...G.#
                #######
                """;
        Cavern cavern = new Cavern(input);
        doRound(cavern);
    }

}
