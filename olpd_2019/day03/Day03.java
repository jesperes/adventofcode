import static java.lang.Math.*;

import java.io.*;
import java.util.*;
import java.util.stream.*;

/**
 * Slightly modified version of the day 3 solution found in the
 * aoc_java project, this one has been modified to be more standalone
 * (single file, no JUnit, etc).
 */
public class Day03 {

    private Map<Character, IntPair> deltas = new HashMap<>();

    public static class Pair<T0, T1> {
        public T0 x;
        public T1 y;

        public Pair(T0 first, T1 second) {
            this.x = first;
            this.y = second;
        }

        @Override
        public int hashCode() {
            return x.hashCode() ^ y.hashCode();
        }

        @Override
        public boolean equals(Object obj) {
            @SuppressWarnings("unchecked")
                Pair<T0, T1> other = (Pair<T0, T1>) obj;
            return other.x.equals(x) && other.y.equals(y);
        }

        @Override
        public String toString() {
            return String.format("{%s,%s}", x, y);
        }

    }

    public static class IntPair extends Pair<Integer, Integer> {
        public IntPair(Integer first, Integer second) {
            super(first, second);
        }

        public static IntPair pair(int x, int y) {
            return new IntPair(x, y);
        }

        public IntPair copy() {
            return new IntPair(x, y);
        }
    }

    public Day03() throws IOException {
        deltas.put('R', new IntPair(1, 0));
        deltas.put('L', new IntPair(-1, 0));
        deltas.put('U', new IntPair(0, 1));
        deltas.put('D', new IntPair(0, -1));
    }


    protected final List<String> getInputAsLines() throws IOException {
        try (BufferedReader r =
             new BufferedReader
             (new FileReader("../inputs/input03.txt"))) {
            return r.lines().collect(Collectors.toList());
        }
    }

    private List<Pair<Character, Integer>> parseLine(String line) {
        return Arrays.stream(line.split(","))
                .map(s -> new Pair<Character, Integer>(s.charAt(0),
                        Integer.valueOf(s.substring(1))))
                .collect(Collectors.toList());
    }

    /*
     * Part 1: traces the wire across the grid, and returns the distance of the
     * closest intersection found.
     */
    private int traceWire(Map<IntPair, Integer> grid,
            List<Pair<Character, Integer>> wire, int wirenum) {

        int closestIntersection = Integer.MAX_VALUE;

        // Initial position is "shared", and does not count for a collision.
        IntPair pos = IntPair.pair(0, 0);

        for (Pair<Character, Integer> instr : wire) {
            IntPair delta = deltas.get(instr.x);
            for (int i = 0; i < instr.y; i++) {
                pos.x += delta.x;
                pos.y += delta.y;
                if (grid.containsKey(pos)) {
                    if (grid.get(pos) == wirenum) {
                        // The wire crosses itself, ignore
                        continue;
                    } else {
                        int manhattan = abs(pos.x) + abs(pos.y);
                        closestIntersection = min(manhattan,
                                closestIntersection);
                    }
                } else {
                    grid.put(pos.copy(), wirenum);
                }
            }
        }

        return closestIntersection;
    }

    private int part1(List<String> wires) throws IOException {
        Map<IntPair, Integer> grid = new HashMap<>();
        List<Pair<Character, Integer>> wire1 = parseLine(wires.get(0));
        List<Pair<Character, Integer>> wire2 = parseLine(wires.get(1));
        traceWire(grid, wire1, 1);
        return traceWire(grid, wire2, 2);
    }

    /*
     * For part 2, "closest" is measured in combined "signal delay" (steps
     * needed to reach the intersection), not manhattan distance.
     */

    class Cell {
        int first; // the wire which came first
        int signalDelay; // the (combined) signal delay

        public Cell(int first, int signalDelay) {
            this.first = first;
            this.signalDelay = signalDelay;
        }
    }

    private int traceWirePart2(Map<IntPair, Cell> grid,
            List<Pair<Character, Integer>> wire, int wirenum) {

        // Initial position is "shared", and does not count for a collision.
        IntPair pos = IntPair.pair(0, 0);

        int signalDelay = 0;
        int shortest = Integer.MAX_VALUE;

        for (Pair<Character, Integer> instr : wire) {
            IntPair delta = deltas.get(instr.x);
            for (int i = 0; i < instr.y; i++) {
                pos.x += delta.x;
                pos.y += delta.y;
                signalDelay++;

                if (grid.containsKey(pos)) {
                    Cell cell = grid.get(pos);
                    if (cell.first == wirenum) // ignore self-crossings
                        continue;

                    cell.signalDelay += signalDelay;
                    shortest = min(shortest, cell.signalDelay);
                } else {
                    grid.put(pos.copy(), new Cell(wirenum, signalDelay));
                }
            }
        }

        return shortest;
    }

    private int part2(List<String> wires) {
        Map<IntPair, Cell> grid = new HashMap<>();
        List<Pair<Character, Integer>> wire1 = parseLine(wires.get(0));
        List<Pair<Character, Integer>> wire2 = parseLine(wires.get(1));
        traceWirePart2(grid, wire1, 1);
        return traceWirePart2(grid, wire2, 2);
    }

    private static void assertEquals(int expected, int actual) {
        if (expected != actual) {
            throw new AssertionError
                (String.format("Expected %d, got %d",
                               expected, actual));
        }
    }

    public void testPart1() throws Exception {
        assertEquals(627, part1(getInputAsLines()));
    }

    public void testPart2() throws Exception {
        assertEquals(13190, part2(getInputAsLines()));
    }

    public void main() throws Exception {
        testPart1();
        testPart2();
    }

    public static void main(String[] argv) throws Exception {
        new Day03().main();
    }
}
