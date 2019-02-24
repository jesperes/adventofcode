package puzzle15;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.LinkedList;
import java.util.List;
import java.util.Optional;
import java.util.Queue;
import java.util.Set;
import java.util.TreeSet;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.junit.Test;

/*
 * Alternate approach.
 */
public class Puzzle15 {

    class ElfDeathException extends Exception {
    }

    static final boolean TRACE = false;
    static int ELF_ATTACK_POWER = 3;

    @FunctionalInterface
    interface PosCallback {
        void call(char c, int x, int y);
    }

    class Grid {
        char[][] grid;

        // The collection of elves and goblins still alive
        List<Unit> units = new ArrayList<>();

        Grid(String filename) throws FileNotFoundException, IOException {
            try (BufferedReader r = new BufferedReader(
                    new FileReader(filename))) {
                parseInput(r.lines().collect(Collectors.toList()));
            }
        }

        // Return a stream of all positions adjacent to the given position.
        // in reading order.
        private Stream<Position> getAdjacent(Position pos) {
            return Stream.of(new Position(pos.x, pos.y - 1),
                    new Position(pos.x - 1, pos.y),
                    new Position(pos.x + 1, pos.y),
                    new Position(pos.x, pos.y + 1));
        }

        private void parseInput(List<String> lines) {
            int height = lines.size();
            int width = lines.get(0).length();

            this.grid = new char[height][width];
            for (int y = 0; y < height; y++) {
                for (int x = 0; x < width; x++) {
                    char c = lines.get(y).charAt(x);
                    switch (c) {
                    case 'G':
                        units.add(new Goblin(this, x, y));
                        grid[y][x] = '.';
                        break;
                    case 'E':
                        units.add(new Elf(this, x, y));
                        grid[y][x] = '.';
                        break;
                    case '.':
                    case '#':
                        grid[y][x] = c;
                        break;
                    }
                }
            }
        }

        // Returns a sorted copy of the list of units
        public Stream<Unit> getSortedUnits() {
            List<Unit> copy = new ArrayList<>();
            copy.addAll(units);
            Collections.sort(copy);
            return copy.stream();
        }

        public boolean isOpen(Position p) {
            if (grid[p.y][p.x] != '.')
                return false;

            for (Unit unit : units) {
                if (unit.equals(p))
                    return false;
            }
            return true;
        }

        @Override
        public String toString() {
            return toString(Collections.emptyList());
        }

        public String toString(List<PathElem> additional) {
            StringBuilder buf = new StringBuilder();

            int height = grid.length;
            int width = grid[0].length;

            buf.append("   ");
            for (int x = 0; x < width; x++) {
                if (x >= 10)
                    break;

                buf.append(String.format("%d", x));
            }
            buf.append("\n");

            for (int y = 0; y < height; y++) {
                int y0 = y;

                buf.append(String.format("%2d ", y));

                for (int x = 0; x < width; x++) {
                    int x0 = x;
                    // display the unit at position, or grid if no unit
                    buf.append(Stream
                            .concat(units.stream(), additional.stream())
                            .filter(unit -> unit.x == x0 && unit.y == y0)
                            .map(unit -> unit.getChar()).findFirst()
                            .orElse(grid[y][x]));
                }

                buf.append(" ");
                /*
                 * We need to sort the units here, otherwise they might end up
                 * in a different order here than when displayed.
                 */
                buf.append(units.stream().filter(unit -> unit.y == y0).sorted()
                        .map(unit -> unit.toString())
                        .collect(Collectors.joining(", ")));

                buf.append("\n");
            }

            return buf.toString();
        }

        public Unit getUnitAt(Position other) {
            for (Unit unit : units) {
                if (unit.equals(other))
                    return unit;
            }
            return null;
        }

        public void kill(Unit enemyUnit) {
            units.remove(enemyUnit);
        }
    }

    static class Position implements Comparable<Position> {
        int x;
        int y;

        public Position(int x, int y) {
            this.x = x;
            this.y = y;
        }

        @Override
        public String toString() {
            return String.format("{%d,%d}", y, x);
        }

        @Override
        public boolean equals(Object obj) {
            Position other = (Position) obj;
            return x == other.x && y == other.y;
        }

        public boolean isAdjacentTo(Position other) {
            if ((other.x == x && Math.abs(other.y - y) == 1)
                    || (other.y == y && Math.abs(other.x - x) == 1))
                return true;

            return false;
        }

        @Override
        public int compareTo(Position o) {
            if (y == o.y) {
                return Integer.compare(x, o.x);
            } else {
                return Integer.compare(y, o.y);
            }
        }

        public char getChar() {
            return '.';
        }

        public void move(Position newPos) {
            if (TRACE)
                System.out.format("[MOVE] %s -> %s%n", this, newPos);
            x = newPos.x;
            y = newPos.y;
        }
    }

    abstract class Unit extends Position {
        int hp = 200;
        final Grid grid;

        public Unit(Grid grid, int x, int y) {
            super(x, y);
            this.grid = grid;
        }

        @Override
        public String toString() {
            return String.format("{%c,%s,%dhp}", getChar(), super.toString(),
                    hp);
        }

        abstract boolean isEnemy(Position other);

        abstract int attackPower();

        // Attack enemy unit, return true if it dies.
        public boolean attack(Unit enemyUnit) {
            enemyUnit.hp -= attackPower();
            return (enemyUnit.hp <= 0);
        }

    }

    class Goblin extends Unit {

        public Goblin(Grid grid, int x, int y) {
            super(grid, x, y);
        }

        @Override
        public char getChar() {
            return 'G';
        }

        @Override
        boolean isEnemy(Position other) {
            Unit unit = grid.getUnitAt(other);
            return unit != null && unit instanceof Elf;
        }

        @Override
        int attackPower() {
            return 3;
        }
    }

    class Elf extends Unit {

        public Elf(Grid grid, int x, int y) {
            super(grid, x, y);
        }

        @Override
        public char getChar() {
            return 'E';
        }

        @Override
        boolean isEnemy(Position other) {
            Unit unit = grid.getUnitAt(other);
            return unit != null && unit instanceof Goblin;
        }

        @Override
        int attackPower() {
            return ELF_ATTACK_POWER;
        }
    }

    static class PathElem extends Position {
        int dist; // distance from source
        PathElem prev;

        public PathElem(int x, int y, int dist, PathElem prev) {
            super(x, y);
            this.dist = dist;
            this.prev = prev;
        }

        @Override
        public char getChar() {
            return '*';
        }
    }

    static class Path implements Comparable<Path> {
        Unit source;
        Position target;

        public int length;
        public List<PathElem> positions = new ArrayList<>();

        public Path(Unit source, Position target, int length) {
            super();
            this.source = source;
            this.target = target;
            this.length = length;
        }

        @Override
        public String toString() {
            return String.format("path from %s to %s (length %d) with steps %s",
                    source, target, length, positions);
        }

        public Position startPos() {
            if (positions.size() == 0)
                return target;
            else
                return positions.get(0);
        }

        @Override
        public int compareTo(Path o) {
            if (length != o.length) {
                return Integer.compare(length, o.length);
            } else {
                if (target.equals(o.target)) {
                    // same target, break ties on start square
                    return startPos().compareTo(o.startPos());
                } else {
                    // different target, break ties on target square
                    return target.compareTo(o.target);
                }
            }
        }
    }

    class GameEngine {
        private Grid grid;
        int currentRound = 0;
        int roundsFinished = 0;
        public boolean attackEnabled = true;
        public boolean allowElfDeaths = true;

        public GameEngine(Grid grid) {
            this.grid = grid;

        }

        public int executeRounds() throws ElfDeathException, IOException {
            currentRound = 1;

            try (BufferedWriter traceFile = new BufferedWriter(
                    new FileWriter("javaoutput.terms"))) {
                while (executeRound(traceFile)) {
                    currentRound++;
                }

                // System.out.println(
                // "Number of full rounds finished: " + roundsFinished);
                //
                int sumHP = grid.getSortedUnits().map(u -> u.hp)
                        .mapToInt(n -> n).sum();

                // System.out.println("Sum of HP of remaining units: " + sumHP);
                return sumHP * roundsFinished;
            }
        }

        public boolean executeRound(BufferedWriter traceFile)
                throws ElfDeathException, IOException {

            traceFile.write(String.format("{{round,%d},[%s]}%n", currentRound,
                    grid.getSortedUnits()
                            .map(u -> String.format("{{%d,%d},{'%c',%d,%d}}",
                                    u.y, u.x, (u instanceof Elf) ? 'E' : 'G',
                                    u.hp, u.attackPower()))
                            .collect(Collectors.joining(","))));

            if (TRACE) {
                System.out.println("--------------------------------------");
            }

            for (Unit unit : grid.getSortedUnits()
                    .collect(Collectors.toList())) {

                if (TRACE) {
                    System.out.println("-- Begin turn for " + unit);
                    System.out.println(grid);
                }

                if (unit.hp <= 0) {
                    if (TRACE)
                        System.out.println(
                                "Unit was killed before it could take its turn.");
                    continue;
                }

                if (!grid.getSortedUnits().filter(e -> unit.isEnemy(e))
                        .findAny().isPresent()) {
                    if (TRACE)
                        System.out
                                .println("No enemies present, combat is over!");
                    return false;
                }

                Optional<Path> pathToNearestEnemy = selectEnemyToAttack(unit);

                if (!pathToNearestEnemy.isPresent()) {
                    if (TRACE)
                        System.out.println("No paths to enemies found.");
                    continue;
                }

                if (pathToNearestEnemy.get().positions.size() > 0) {
                    if (TRACE) {
                        System.out.println("[MOVE] Moving towards enemy along "
                                + pathToNearestEnemy.get());
                        System.out.println(grid
                                .toString(pathToNearestEnemy.get().positions));
                    }
                    unit.move(pathToNearestEnemy.get().positions.get(0));
                }

                if (attackEnabled) {
                    if (TRACE) {
                        System.out.println("Path to nearest enemy: "
                                + pathToNearestEnemy.get());
                    }

                    // Adjacent to at least one enemy. Select the one
                    // with lowest hp, break ties on reading order.
                    Optional<Position> toAttack = grid.getAdjacent(unit)
                            .filter(e -> unit.isEnemy(e))
                            // .peek(System.out::println)
                            .min(attackComparator);

                    if (toAttack.isPresent()) {
                        Position pos = toAttack.get();
                        Unit enemyUnit = grid.getUnitAt(pos);

                        if (TRACE)
                            System.out.println(
                                    "[ATTACK] Attacking enemy: " + enemyUnit);

                        if (unit.attack(enemyUnit)) {

                            if (enemyUnit instanceof Elf && !allowElfDeaths) {
                                throw new ElfDeathException();
                            }

                            grid.kill(enemyUnit);

                            char c = (enemyUnit instanceof Elf) ? 'E' : 'G';

                            traceFile.write(String.format(
                                    "{{dead,'%c'},{pos,{%d,%d}}}%n", c,
                                    enemyUnit.y, enemyUnit.x));

                        } else {
                            if (TRACE)
                                System.out.println(
                                        "[ATTACK] Enemy unit: " + enemyUnit);
                        }
                    } else {
                        if (TRACE)
                            System.out.println(
                                    "[ATTACK] Nothing to attack here.");
                    }
                }
            }

            if (TRACE) {
                System.out.format("==== After round %d ====%n", currentRound);
                System.out.println(grid);
            }

            roundsFinished++;
            return true;
        }

        Comparator<Position> attackComparator = new Comparator<Position>() {
            @Override
            public int compare(Position o1, Position o2) {
                Unit u1 = grid.getUnitAt(o1);
                Unit u2 = grid.getUnitAt(o2);

                if (u1.hp == u2.hp) {
                    return u1.compareTo(u2); // reading order
                } else {
                    return Integer.compare(u1.hp, u2.hp);
                }
            }
        };
        Comparator<Path> pathComparator = new Comparator<Path>() {
            @Override
            public int compare(Path o1, Path o2) {
                return o1.compareTo(o2);
            }
        };

        public Optional<Path> selectEnemyToAttack(Unit unit) {
            return grid.getSortedUnits().filter(other -> unit.isEnemy(other))
                    .flatMap(enemy -> grid.getAdjacent(enemy))
                    .map(adj -> findShortestPath(unit, adj))
                    .filter(opt -> opt.isPresent()).map(opt -> opt.get())
                    .min(pathComparator);
        }

        // Find the shortest path from 'source' to 'target'.
        public Optional<Path> findShortestPath(Unit source, Position target) {
            // System.out.format("Finding shortest path from %s to %s%n",
            // source,
            // target);

            Set<Position> visited = new TreeSet<>(); // closed set
            Queue<PathElem> queue = new LinkedList<>(); // open set

            queue.add(new PathElem(source.x, source.y, 0, null));

            while (!queue.isEmpty()) {
                PathElem p = queue.poll();

                // System.out.println("Queue size: " + queue.size());

                if (p.equals(target)) {
                    /*
                     * We have reached our target. Construct a path by
                     * traversing the path elements backwards.
                     */
                    Path shortestPath = new Path(source, target, p.dist);

                    while (true) {
                        if (p.prev == null) {
                            break;
                        } else {
                            shortestPath.positions.add(p);
                            p = p.prev;
                        }
                    }

                    Collections.reverse(shortestPath.positions);
                    // System.out.format(
                    // "Found shortest path from %s to %s with length %d and
                    // steps: %s%n",
                    // source, target, shortestPath.length,
                    // shortestPath.positions);
                    // System.out.println(grid.toString(shortestPath.positions));
                    return Optional.of(shortestPath);
                } else {
                    PathElem p0 = p;

                    visited.add(p);

                    grid.getAdjacent(p0)
                            .filter(a -> (grid.isOpen(a) && !queue.contains(a)
                                    && !visited.contains(a)))
                            .map(a -> new PathElem(a.x, a.y, p0.dist + 1, p0))
                            .forEach(a -> queue.add(a));
                }
            }

            return Optional.empty();
        }
    }

    public int runTestCase(String filename)
            throws FileNotFoundException, IOException, ElfDeathException {
        return runTestCase(filename, true);
    }

    public int runTestCase(String filename, boolean allowElfDeaths)
            throws FileNotFoundException, IOException, ElfDeathException {
        Grid grid = new Grid(filename);
        GameEngine engine = new GameEngine(grid);
        engine.allowElfDeaths = allowElfDeaths;
        return engine.executeRounds();
    }

    @Test
    public void testInput4() throws Exception {
        assertEquals(27730, runTestCase("testinput4.txt"));
    }

    @Test
    public void testInput5() throws Exception {
        assertEquals(36334, runTestCase("testinput5.txt"));
    }

    @Test
    public void testInput6() throws Exception {
        assertEquals(39514, runTestCase("testinput6.txt"));
    }

    @Test
    public void testInput7() throws Exception {
        assertEquals(27755, runTestCase("testinput7.txt"));
    }

    @Test
    public void testInput8() throws Exception {
        assertEquals(28944, runTestCase("testinput8.txt"));
    }

    @Test
    public void testInput9() throws Exception {
        assertEquals(18740, runTestCase("testinput9.txt"));
    }

    @Test
    public void testInput10() throws Exception {
        assertEquals(10804, runTestCase("testinput10.txt"));
    }

    @Test
    public void testPart1() throws Exception {
        assertEquals(237996, runTestCase("input.txt"));
    }

    @Test
    public void testPart2Input4() throws Exception {
        ELF_ATTACK_POWER = 15;
        try {
            assertEquals(4988, runTestCase("testinput4.txt", false));
        } catch (ElfDeathException e) {
            fail("An elf died!");
        }
    }

    @Test
    public void testPart2RealInput() throws Exception {
        ELF_ATTACK_POWER = 4;

        while (true) {
            try {
                System.out.println("ELF_ATTACK_POWER: " + ELF_ATTACK_POWER);
                int outcome = runTestCase("input.txt", false);
                System.out.format("Elf attack power %d was super effective!%n",
                        ELF_ATTACK_POWER);
                System.out.println("Outcome: " + outcome);
                break;
            } catch (ElfDeathException e) {
                System.out.format(
                        "Elf attack power %d was too feeble, increasing... %n",
                        ELF_ATTACK_POWER);

                ELF_ATTACK_POWER++;
            }
        }
    }
}
