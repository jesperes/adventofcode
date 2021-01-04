package aoc2020.solutions;

import java.io.File;
import java.io.FileNotFoundException;
import java.util.Optional;

import aoc2020.AocBaseRunner;
import aoc2020.AocPuzzleInfo;
import aoc2020.AocResult;
import aoc2020.IAocIntPuzzle;
import aoc2020.solutions.Day23.Cups;

public class Day23 implements IAocIntPuzzle<Cups> {

    record Cups(int[] ring, int[] input) {
    }

    @Override
    public AocPuzzleInfo getInfo() {
        return new AocPuzzleInfo(2020, 23, "Crab Cups", false);
    }

    @Override
    public AocResult<Long, Long> getExpected() {
        return AocResult.of(28946753L, 519044017360L);
    }

    @Override
    public Cups parse(Optional<File> file) {
        /*
         * Represent the ring of cups as an integer array. Java's collection
         * types are too expensive; we would need to create 1M objects (for part
         * 2).
         */
        int[] input = new int[] { 5, 8, 6, 4, 3, 9, 1, 7, 2 };
        int[] ring = new int[input.length + 1];
        for (int i = 0; i < input.length - 1; i++) {
            ring[input[i]] = input[i + 1];
        }
        ring[input[input.length - 1]] = input[0];
        return new Cups(ring, input);
    }

    @Override
    public Long part1(Cups input) {
        int[] ring = input.ring.clone();
        repeat(ring, input.input[0], 100, 9);
        return labels(ring, 1);
    }

    @Override
    public Long part2(Cups input) {
        var size = 1_000_000;
        var repeats = 10_000_000;

        // Make a 1M integer array to store the ring in as a linked list.
        int[] ring = new int[size + 1];

        // Fill it up with integers
        var fillStart = input.input.length + 1;
        for (int i = 0; i < input.ring.length; i++) {
            ring[i] = input.ring[i];
        }
        int last = input.input[input.input.length - 1];
        ring[last] = fillStart;
        for (int i = fillStart; i < ring.length - 1; i++) {
            ring[i] = i + 1;
        }
        ring[ring.length - 1] = input.input[0];

        // Play crab cups for 10M rounds.
        repeat(ring, input.input[0], repeats, size);

        int a = ring[1];
        int b = ring[a];
        return (long) a * (long) b;
    }

    private static long labels(int[] cups, int start) {
        StringBuilder b = new StringBuilder();
        for (int i = cups[start]; i != start; i = cups[i]) {
            b.append(i);
        }
        return Long.parseLong(b.toString());
    }

    private static void repeat(int[] ring, int current, int repeat, int max) {
        for (long i = 0; i < repeat; i++) {
            var a = ring[current];
            var b = ring[a];
            var c = ring[b];
            var next = ring[c];
            ring[a] = -1;
            ring[b] = -1;
            ring[c] = -1;
            ring[current] = next;
            var dest = current;
            do {
                dest = (dest == 1) ? max : dest - 1;
            } while (ring[dest] == -1);
            var destNext = ring[dest];
            ring[dest] = a;
            ring[a] = b;
            ring[b] = c;
            ring[c] = destNext;
            current = next;
        }
    }

    public static void main(String[] args) throws FileNotFoundException {
        AocBaseRunner.run(new Day23());
    }

}
