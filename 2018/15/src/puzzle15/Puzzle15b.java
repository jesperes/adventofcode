package puzzle15;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.io.BufferedReader;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;
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
public class Puzzle15b {
    static class Grid {
        char[][] grid;

        // The collection of elves and goblins still alive
        Collection<Unit> units = new ArrayList<>();

        Grid(String filename) throws FileNotFoundException, IOException {
            try (BufferedReader r = new BufferedReader(
                    new FileReader(filename))) {
                parseInput(r.lines().collect(Collectors.toList()));
            }
        }

        @FunctionalInterface
        interface PosCallback {
            void call(char c, int x, int y);
        }

        // Invoke callback for each non-wall position on the grid.
        private Stream<Position> getSortedPositions() {
            int height = grid.length;
            int width = grid[0].length;
            List<Position> pos = new ArrayList<>();

            for (int y = 0; y < height; y++) {
                for (int x = 0; x < width; x++) {
                    if (grid[y][x] != '#') {
                        pos.add(new Position(x, y));
                    }
                }
            }

            return pos.stream();
        }

        // Return a stream of all positions adjacent to the given position.
        private Stream<Position> getAdjacent(Position pos) {
            return Stream.of(new Position(pos.x - 1, pos.y),
                    new Position(pos.x + 1, pos.y),
                    new Position(pos.x, pos.y + 1),
                    new Position(pos.x, pos.y - 1));
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
                buf.append(
                        units.stream().filter(unit -> unit.y == y0)
                                .map(unit -> String.format("%c (%s)",
                                        unit.getChar(), unit.hp))
                                .collect(Collectors.joining(", ")));

                buf.append("\n");
            }

            return buf.toString();
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
            return String.format("{%d,%d}", x, y);
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
            System.out.format("[MOVE] %s -> %s%n", this, newPos);
            x = newPos.x;
            y = newPos.y;
        }
    }

    static abstract class Unit extends Position {
        int hp = 200;
        static final int ATTACK_POWER = 3;
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

        abstract boolean isEnemy(Unit other);

    }

    static class Goblin extends Unit {

        public Goblin(Grid grid, int x, int y) {
            super(grid, x, y);
        }

        @Override
        public char getChar() {
            return 'G';
        }

        @Override
        boolean isEnemy(Unit other) {
            return other instanceof Elf;
        }
    }

    static class Elf extends Unit {

        public Elf(Grid grid, int x, int y) {
            super(grid, x, y);
        }

        @Override
        public char getChar() {
            return 'E';
        }

        @Override
        boolean isEnemy(Unit other) {
            return other instanceof Goblin;
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

        @Override
        public String toString() {
            return String.format("path from %s to %s (length %d) with steps %s",
                    source, target, length, positions);
        }

        @Override
        public int compareTo(Path o) {
            if (length != o.length) {
                return Integer.compare(length, o.length);
            } else {
                if (target.equals(o.target)) {
                    // same target, break ties on start square
                    return positions.get(0).compareTo(o.positions.get(0));
                } else {
                    // different target, break ties on target square
                    return target.compareTo(o.target);
                }
            }
        }
    }

    static class GameEngine {
        private Grid grid;
        int round = 1;

        public GameEngine(Grid grid) {
            this.grid = grid;

        }

        public void executeRound() {
            System.out.println("Executing round " + round);
            System.out.println("--------------------------------------");

            for (Unit unit : grid.getSortedUnits()
                    .collect(Collectors.toList())) {
                System.out.println();
                System.out.println("-- Begin turn for " + unit);

                if (unit.hp <= 0) {
                    System.out.println(
                            "Unit was killed before it could take its turn.");
                    continue;
                }

                Optional<Path> pathToNearestEnemy = selectEnemyToAttack(unit);
                if (pathToNearestEnemy.isPresent()) {
                    System.out.println("[ATTACK] Attacking along "
                            + pathToNearestEnemy.get());
                    unit.move(pathToNearestEnemy.get().positions.get(0));
                }
            }
        }

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
            Set<Position> visited = new TreeSet<>(); // closed set
            Queue<PathElem> queue = new LinkedList<>(); // open set

            queue.add(new PathElem(source.x, source.y, 0, null));

            while (!queue.isEmpty()) {

                PathElem p = queue.poll();
                if (p.equals(target)) {
                    /*
                     * We have reached our target. Construct a path by
                     * traversing the path elements backwards.
                     */
                    Path shortestPath = new Path();
                    shortestPath.length = p.dist;
                    shortestPath.source = source;
                    shortestPath.target = target;

                    while (true) {
                        if (p.prev == null) {
                            break;
                        } else {
                            shortestPath.positions.add(p);
                            p = p.prev;
                        }
                    }

                    Collections.reverse(shortestPath.positions);
                    System.out.format(
                            "Found shortest path from %s to %s with length %d%n",
                            source, target, shortestPath.length);
                    System.out.println(grid.toString(shortestPath.positions));
                    return Optional.of(shortestPath);
                } else {
                    PathElem p0 = p;

                    visited.add(p);

                    grid.getAdjacent(p0).filter(a -> (grid.isOpen(a)
                            // && !queue.contains(a)
                            && !visited.contains(a)))
                            .map(a -> new PathElem(a.x, a.y, p0.dist + 1, p0))
                            .forEach(a -> queue.add(a));
                }
            }

            return Optional.empty();
        }
    }

    @Test
    public void testAdjacent() throws Exception {
        Grid grid = new Grid("input.txt");
        Position p1 = new Position(14, 1);
        Position p2 = new Position(14, 2);
        assertTrue(p1.isAdjacentTo(p2));
        grid.getAdjacent(new Position(14, 1)).forEach(System.out::println);
    }

    @Test
    public void testGameEngine() throws Exception {
        Grid grid = new Grid("input.txt");
        GameEngine engine = new GameEngine(grid);
        Unit first = grid.getSortedUnits().findFirst().get();
        System.out.println(
                "Finding possible squares adjacent to targets for " + first);
        engine.selectEnemyToAttack(first);
    }

    @Test
    public void testShortestPath() throws Exception {
        Grid grid = new Grid("testinput.txt");
        GameEngine engine = new GameEngine(grid);
        Goblin g1 = new Goblin(grid, 1, 1);
        Position p = new Position(6, 6);
        Optional<Path> shortestPath = engine.findShortestPath(g1, p);
        assertEquals(10, shortestPath.get().length);
    }

    @Test
    public void testMove() throws Exception {
        Grid grid = new Grid("testinput.txt");
        GameEngine engine = new GameEngine(grid);
        engine.executeRound();
        System.out.println(grid);
    }
}
