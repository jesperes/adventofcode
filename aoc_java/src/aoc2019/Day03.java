package aoc2019;

import static common.IntPair.pair;
import static java.lang.Math.abs;
import static java.lang.Math.min;
import static org.junit.Assert.assertEquals;

import java.io.IOException;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import org.junit.Test;

import common.AocPuzzle;
import common.IntPair;
import common.Pair;

/**
 * AoC puzzle template.
 */
public class Day03 extends AocPuzzle {

    private Map<Character, IntPair> deltas = new HashMap<>();

    public Day03() throws IOException {
        super(2019, 3);
        deltas.put('R', new IntPair(1, 0));
        deltas.put('L', new IntPair(-1, 0));
        deltas.put('U', new IntPair(0, 1));
        deltas.put('D', new IntPair(0, -1));
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
        IntPair pos = pair(0, 0);

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
        IntPair pos = pair(0, 0);

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

    /*
     * Tests
     */

    @Test
    public void testPart1() throws Exception {
        assertEquals(627, part1(getInputAsLines()));
    }

    @Test
    public void testPart2() throws Exception {
        assertEquals(13190, part2(getInputAsLines()));
    }

    @Test
    public void testExample1() throws Exception {
        List<String> list = Arrays.asList("R8,U5,L5,D3", "U7,R6,D4,L4");
        assertEquals(6, part1(list));
        assertEquals(30, part2(list));
    }

    @Test
    public void testExample2() throws Exception {
        List<String> list = Arrays.asList("R75,D30,R83,U83,L12,D49,R71,U7,L72",
                "U62,R66,U55,R34,D71,R55,D58,R83");
        assertEquals(159, part1(list));
        assertEquals(610, part2(list));
    }

    @Test
    public void testExample3() throws Exception {
        List<String> list = Arrays.asList(
                "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51",
                "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7");
        assertEquals(135, part1(list));
        assertEquals(410, part2(list));
    }

}
