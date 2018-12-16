package puzzle15;

import static org.junit.Assert.assertEquals;

import java.io.BufferedReader;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Queue;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;
import java.util.stream.Collectors;

import org.junit.Test;

public class Puzzle15 {

    static int ATTACK_POWER = 3;

    private static class Pos implements Comparable<Pos> {
        final int x;
        final int y;

        public Pos(int x, int y) {
            super();
            this.x = x;
            this.y = y;
        }

        @Override
        public String toString() {
            return String.format("{%d,%d}", x, y);
        }

        public boolean equals(Object o) {
            Pos p = (Pos) o;
            return p.x == x && p.y == y;
        }

        public int hashCode() {
            return Integer.hashCode(x) ^ Integer.hashCode(y);
        }

        public char getAtPos(char[][] grid) {
            return grid[y][x];
        }

        @Override
        public int compareTo(Pos o) {
            /*
             * Sort positions by reading order: top-to-bottom then left-to-right
             */
            if (y != o.y) {
                return Integer.compare(y, o.y);
            } else {
                return Integer.compare(x, o.x);
            }
        }
    }

    private static class Path implements Comparable<Path> {
        final public Pos startsAt;
        final public Pos target;
        final public int length;

        public Path(Pos startsAt, Pos target, int length) {
            super();
            this.startsAt = startsAt;
            this.target = target;
            this.length = length;
        }

        @Override
        public int compareTo(Path o) {
            int i0 = Integer.compare(length, o.length);
            if (i0 != 0) {
                return i0;
            } else {
                int i1 = target.compareTo(o.target);
                if (i1 != 0) {
                    return i1;
                } else {
                    return startsAt.compareTo(o.startsAt);
                }
            }
        }

        @Override
        public String toString() {
            return "Path [startsAt=" + startsAt + ", target=" + target
                    + ", length=" + length + "]";
        }

        @Override
        public boolean equals(Object obj) {
            Path o = (Path) obj;
            return startsAt.equals(o.startsAt) && target.equals(o.target);
        }
    }

    public static void main(String[] args)
            throws FileNotFoundException, IOException {
    }

    @Test
    public void testScenario4() throws Exception {
        assertEquals(27730, runCombat("testinput4.txt"));
    }

    @Test
    public void testScenario5() throws Exception {
        assertEquals(36334, runCombat("testinput5.txt"));
    }

    @Test
    public void testScenario6() throws Exception {
        assertEquals(39514, runCombat("testinput6.txt"));
    }

    @Test
    public void testScenario7() throws Exception {
        assertEquals(27755, runCombat("testinput7.txt"));
    }

    @Test
    public void testScenario8() throws Exception {
        assertEquals(28944, runCombat("testinput8.txt"));
    }

    @Test
    public void testScenario9() throws Exception {
        assertEquals(18740, runCombat("testinput9.txt"));
    }

    @Test
    public void testScenario10() throws Exception {
        assertEquals(10804, runCombat("testinput10.txt"));
    }

    @Test
    public void testPart1Real() throws Exception {
        assertEquals(0, runCombat("input.txt"));
    }

    private static int runCombat(String filename)
            throws FileNotFoundException, IOException {
        char[][] grid = parseInput(filename);
        Map<Pos, Integer> hitpoints = new TreeMap<>();
        int height = grid.length;
        int width = grid[0].length;
        for (int y = 0; y < height; y++) {
            for (int x = 0; x < width; x++) {
                char c = grid[y][x];
                if (c == 'G' || c == 'E') {
                    hitpoints.put(new Pos(x, y), 200);
                }
            }
        }

        System.out.println("Initial state:");
        printGrid(grid, hitpoints);
        System.out.println(hitpoints);
        for (int i = 1; true; i++) {

            System.out.format("Executing round %d%n", i);
            if (!executeRound(grid, hitpoints)) {
                int numFullRounds = i - 1;
                System.out.println("No more moves. Number of full rounds: "
                        + numFullRounds);

                int sumHP = hitpoints.values().stream().mapToInt(n -> n).sum();
                System.out.println("Sum HP: " + sumHP);
                int outcome = sumHP * numFullRounds;
                System.out.println("Outcome: " + outcome);
                return outcome;
            }

            System.out.format("After %d rounds:%n", i);
            printGrid(grid, hitpoints);
            System.out.println(hitpoints);
        }
    }

    private static void printGrid(char[][] grid, Map<Pos, Integer> hitpoints) {
        printGrid(grid, hitpoints, Collections.emptyList(), (char) 0);
    }

    private static void printGrid(char[][] grid, Map<Pos, Integer> hitpoints,
            List<Pos> plist, char plistc) {
        System.out.println("HP: " + hitpoints);
        int height = grid.length;
        int width = grid[0].length;

        for (int t = 1; t > 0; t /= 10) {
            System.out.print("   ");
            for (int x = 0; x < width; x++) {
                System.out.format("%d", x / t);
            }
            System.out.println();
        }

        for (int y = 0; y < height; y++) {
            System.out.format("%2d ", y);
            for (int x = 0; x < width; x++) {
                if (plist.contains(new Pos(x, y))) {
                    System.out.print(plistc);
                } else {
                    System.out.print(grid[y][x]);
                }
            }
            for (Entry<Pos, Integer> e : hitpoints.entrySet()) {
                Pos pos = e.getKey();

                if (pos.y == y) {
                    int hp = e.getValue();
                    char type = grid[pos.y][pos.x];
                    System.out.format(" %c(%d)", type, hp);
                }
            }

            System.out.println();
        }
    }

    private static char[][] parseInput(String filename)
            throws FileNotFoundException, IOException {
        try (BufferedReader r = new BufferedReader(new FileReader(filename))) {
            List<String> lines = r.lines().collect(Collectors.toList());

            int height = lines.size();
            int width = lines.get(0).length();

            char[][] grid = new char[height][width];
            for (int y = 0; y < height; y++) {
                for (int x = 0; x < width; x++) {
                    grid[y][x] = lines.get(y).charAt(x);
                }
            }

            System.out.format("Parsed grid of size %dx%d%n", grid[0].length,
                    grid.length);
            return grid;
        }
    }

    private static List<Pos> getUnitsInOrder(char[][] grid, char... type) {
        List<Pos> list = new ArrayList<>();
        for (int y = 0; y < grid.length; y++) {
            for (int x = 0; x < grid[y].length; x++) {
                char c = grid[y][x];
                for (char t : type) {
                    if (c == t)
                        list.add(new Pos(x, y));
                }
            }
        }
        return list;

    }

    private static char enemy(char c) {
        if (c == 'G')
            return 'E';
        else
            return 'G';
    }

    // Returns all positions adjacent to a position
    private static List<Pos> adjacent(char[][] grid, Pos pos) {
        List<Pos> plist = new ArrayList<>();
        plist.add(pos);
        return adjacent(grid, plist);
    }

    // Return the list of all positions adjacent to plist
    private static List<Pos> adjacent(char[][] grid, List<Pos> plist) {
        List<Pos> list = new ArrayList<>();
        adjacent(grid, plist, list);
        return list;
    }

    // Add all positions to list which are adjacent to any position in plist
    private static void adjacent(char[][] grid, List<Pos> plist,
            List<Pos> list) {
        for (Pos p : plist) {
            adjacentToUnit(grid, list, p);
        }
    }

    // Add (x,y) to the list of positions, unless it is a wall
    private static void add(char[][] grid, List<Pos> list, int x, int y) {
        if (grid[y][x] != '#') {
            list.add(new Pos(x, y));
        }
    }

    // Add all positions which are adjacent to p to the given list.
    private static void adjacentToUnit(char[][] grid, List<Pos> list, Pos p) {
        add(grid, list, p.x, p.y - 1);
        add(grid, list, p.x, p.y + 1);
        add(grid, list, p.x + 1, p.y);
        add(grid, list, p.x - 1, p.y);
    }

    private static boolean executeRound(char[][] grid,
            Map<Pos, Integer> hitpoints) {
        System.out.println("=======[ executeRound() ]=========");

        List<Pos> units = getUnitsInOrder(grid, 'E', 'G');

        for (Pos unit : units) {
            if (!hitpoints.containsKey(unit)) {
                System.out.println(
                        "Unit was killed before it could take its turn: "
                                + unit);
                continue;
            }

            char type = grid[unit.y][unit.x];
            char enemy = enemy(type);
            List<Pos> allEnemyUnits = getUnitsInOrder(grid, enemy);
            if (allEnemyUnits.isEmpty()) {
                return false;
            }

            List<Pos> adjacentToAnyEnemyUnits = adjacent(grid, allEnemyUnits);

            /*
             * If the unit is not adjacent to any enemy units, see if we can
             * move it towards one.
             */
            if (!adjacentToAnyEnemyUnits.contains(unit)) {

                // Find all positions reachable from this unit.
                Set<Pos> reachable = new TreeSet<>();
                addReachable(grid, unit, reachable);

                // Filter out all positions which are not adjacent to an enemy
                // position.
                Set<Pos> reachableAndAdjacent = reachable.stream()
                        .filter(pos -> adjacentToAnyEnemyUnits.contains(pos))
                        .collect(Collectors.toSet());

                List<Path> paths = new ArrayList<>();

                for (Pos p : reachableAndAdjacent) {
                    paths.addAll(findShortestPaths(grid, p, unit, type));
                }

                if (paths.size() > 0) {
                    /*
                     * There is at least one path towards an enemy unit.
                     */
                    Collections.sort(paths);
                    Path shortestPath = paths.get(0);
                    Pos moveTo = shortestPath.startsAt;

                    if (Boolean.TRUE) {
                        Set<Path> allShortestPaths = new TreeSet<>();
                        allShortestPaths.addAll(paths);
                        List<Path> sortedShortedPaths = new ArrayList<>();
                        sortedShortedPaths.addAll(allShortestPaths);
                        Collections.sort(sortedShortedPaths);

                        System.out.format(
                                "All paths from %s at %s to enemies:%n", type,
                                unit);
                        for (Path p : sortedShortedPaths) {
                            System.out.println(p);
                        }
                    }

                    System.out.format(
                            "[MOVE] Moving %s from %s to %s (towards %s)%n",
                            grid[unit.y][unit.x], unit, moveTo,
                            shortestPath.target);

                    int hp = hitpoints.remove(unit);
                    grid[moveTo.y][moveTo.x] = type;
                    grid[unit.y][unit.x] = '.';
                    unit = moveTo;
                    // Bring the hitpoints along
                    hitpoints.put(unit, hp);
                }
            }

            List<Pos> adjacentEnemies = adjacent(grid, unit).stream()
                    .filter(p -> (grid[p.y][p.x] == enemy))
                    .collect(Collectors.toList());

            if (!adjacentEnemies.isEmpty()) {
                // Sort adjacent enemies on hitpoints
                adjacentEnemies.sort(new Comparator<Pos>() {
                    @Override
                    public int compare(Pos o1, Pos o2) {
                        int hp1 = hitpoints.get(o1);
                        int hp2 = hitpoints.get(o2);
                        int c = Integer.compare(hp1, hp2);
                        if (c != 0) {
                            return c;
                        } else {
                            // use reading order
                            return o1.compareTo(o2);
                        }
                    }
                });

                System.out.format(
                        "Unit %s at %s is in range of the following enemies: %s%n",
                        type, unit, adjacentEnemies);

                Pos closestEnemy = adjacentEnemies.get(0);
                int oldhp = hitpoints.get(closestEnemy);
                int newhp = oldhp - ATTACK_POWER;
                System.out.format(
                        "[ATTACK] Unit %s at %s attacks %s at %s, reducing HP from %d to %d%n",
                        type, unit, closestEnemy.getAtPos(grid), closestEnemy,
                        oldhp, newhp);
                hitpoints.put(closestEnemy, newhp);
                if (newhp <= 0) {
                    // Enemy dies
                    grid[closestEnemy.y][closestEnemy.x] = '.';
                    hitpoints.remove(closestEnemy);
                }
            }
        }

        return true;
    }

    private static void addReachable(char[][] grid, Pos unit,
            Set<Pos> reachable) {

        for (Pos adjacentUnit : adjacent(grid, unit)) {
            if (adjacentUnit.getAtPos(grid) != '.')
                continue; // ignore non-open positions

            if (reachable.contains(adjacentUnit)) {
                continue;
            } else {
                reachable.add(adjacentUnit);
                addReachable(grid, adjacentUnit, reachable);
            }
        }
    }

    private static void printGridWithDist(char[][] grid,
            Map<Pos, Integer> dist) {
        for (int y = 0; y < grid.length; y++) {
            for (int x = 0; x < grid[y].length; x++) {
                char c = grid[y][x];
                Integer d = dist.get(new Pos(x, y));
                if (c == '.' && d != null) {
                    System.out.print(d.intValue());
                } else {
                    System.out.print(grid[y][x]);
                }
            }
            System.out.println();
        }
    }

    private static Set<Path> findShortestPaths(char[][] grid, Pos start,
            Pos dest, char type) {

        Pos p = start;
        Map<Pos, Integer> dist = new TreeMap<>();
        Queue<Pos> unvisited = new LinkedList<>();

        dist.put(p, 0);
        unvisited.add(p);

        while (!unvisited.isEmpty()) {
            // System.out.println("Unvisited: " + unvisited);
            // printGridWithDist(grid, dist);

            p = unvisited.poll();

            if (p.equals(dest)) {
                Set<Path> set = new TreeSet<>();

                // printGridWithDist(grid, dist);

                for (Pos apos : adjacent(grid, dest)) {
                    if (apos.getAtPos(grid) == '#')
                        continue;

                    if (!dist.containsKey(apos))
                        continue;

                    int adist = dist.get(apos);
                    set.add(new Path(apos, start, adist));
                }

                return set;
            }

            if (p.getAtPos(grid) != '.') {
                // If the position isn't what we are searching for and isn't
                // open, ignore it.
                continue;
            }

            int adjDist = dist.get(p) + 1;

            for (Pos apos : adjacent(grid, p)) {
                if (dist.containsKey(apos))
                    continue;

                dist.put(apos, adjDist);
                unvisited.add(apos);
            }
        }

        // Target position could not be found.
        return Collections.emptySet();
    }
}
