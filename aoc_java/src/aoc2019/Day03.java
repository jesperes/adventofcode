package aoc2019;

import static common.IntPair.pair;
import static java.lang.Math.abs;
import static java.lang.Math.min;

import java.io.File;
import java.io.IOException;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.stream.Collectors;

import common.IntPair;
import common.Pair;
import common2.AocBaseRunner;
import common2.AocPuzzleInfo;
import common2.AocResult;
import common2.IAocIntPuzzle;
import common2.InputUtils;

/**
 * AoC puzzle template.
 */
public class Day03 implements IAocIntPuzzle<List<String>> {

    private Map<Character, IntPair> deltas = new HashMap<>();

    public Day03() {
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

    @Override
    public AocPuzzleInfo getInfo() {
        return new AocPuzzleInfo(2019, 3, "Crossed Wires", true);
    }

    @Override
    public AocResult<Integer, Integer> getExpected() {
        return AocResult.of(627, 13190);
    }

    @Override
    public List<String> parse(Optional<File> file) throws IOException {
        return InputUtils.asStringList(file.get());
    }

    @Override
    public Integer part1(List<String> wires) {
        Map<IntPair, Integer> grid = new HashMap<>();
        List<Pair<Character, Integer>> wire1 = parseLine(wires.get(0));
        List<Pair<Character, Integer>> wire2 = parseLine(wires.get(1));
        traceWire(grid, wire1, 1);
        return traceWire(grid, wire2, 2);
    }

    @Override
    public Integer part2(List<String> wires) {
        Map<IntPair, Cell> grid = new HashMap<>();
        List<Pair<Character, Integer>> wire1 = parseLine(wires.get(0));
        List<Pair<Character, Integer>> wire2 = parseLine(wires.get(1));
        traceWirePart2(grid, wire1, 1);
        return traceWirePart2(grid, wire2, 2);
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

    public static void main(String[] args) {
        AocBaseRunner.run(new Day03());
    }
}
