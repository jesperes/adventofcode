package aoc2020.solutions;

import static org.junit.Assert.assertEquals;

import java.io.File;
import java.io.FileNotFoundException;
import java.util.HashMap;
import java.util.Map;
import java.util.Optional;
import java.util.stream.Collectors;

import aoc2020.AocBaseRunner;
import aoc2020.AocPuzzleInfo;
import aoc2020.AocResult;
import aoc2020.IAocIntPuzzle;
import aoc2020.solutions.Day23.Cups;

public class Day23 implements IAocIntPuzzle<Cups> {

    record Cups(Map<Long, Long> cups, long current, long last) {
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
        long[] input = { 5L, 8L, 6L, 4L, 3L, 9L, 1L, 7L, 2L };
        Map<Long, Long> map = new HashMap<>();
        for (int i = 0; i < input.length - 1; i++) {
            map.put(input[i], input[i + 1]);
        }
        map.put(input[input.length - 1], input[0]);
        return new Cups(map, input[0], input[input.length - 1]);
    }

    @Override
    public Long part1(Cups input) {
        Map<Long, Long> cups = new HashMap<>();
        cups.putAll(input.cups);
        var current = input.current;
        var stats = input.cups.values().stream()
                .collect(Collectors.summarizingLong(n -> n));
        var max = stats.getMax();
        repeat(cups, current, 100, max);
        return labels(cups, 1);
    }

    private static void repeat(final Map<Long, Long> map, long current,
            final long repeat, final long max) {

        /*
         * TODO rewrite this using a long array instead to avoid unboxing
         */
        for (long i = 0; i < repeat; i++) {
            var a = map.get(current);
            var b = map.get(a);
            var c = map.get(b);
            var next = map.get(c);
            map.remove(a);
            map.remove(b);
            map.remove(c);
            map.put(current, next);
            // var dest = findDest(map, current, max);
            long dest = current;
            do {
                dest = (dest == 1) ? max : dest - 1;
            } while (!map.containsKey(dest));
            var destNext = map.get(dest);
            map.put(dest, a);
            map.put(a, b);
            map.put(b, c);
            map.put(c, destNext);
            current = next;
        }
    }

    private static long labels(Map<Long, Long> map, long start) {
        StringBuilder b = new StringBuilder();
        for (long i = 0,
                n = map.get(start); i < map.size() - 1; i++, n = map.get(n)) {
            b.append(n);
        }
        return Long.parseLong(b.toString());
    }

    @Override
    public Long part2(Cups input) {
        Map<Long, Long> cups = new HashMap<>();
        cups.putAll(input.cups);

        var size = 1_000_000L;
        var repeats = 10_000_000L;
        var current = input.current;
        var max = size;

        // Fill up the rest of the map
        var fillStart = (long) cups.size() + 1;
        cups.put(input.last, fillStart);
        for (long i = fillStart; i < size; i++) {
            cups.put(i, i + 1);
        }
        cups.put(size, current);

        assertEquals(size, cups.size());
        repeat(cups, current, repeats, max);

        var a = cups.get(1L);
        var b = cups.get(a);
        return a * b;
    }

    public static void main(String[] args) throws FileNotFoundException {
        AocBaseRunner.run(new Day23());
    }

}
