package aoc2020.solutions;

import java.io.File;
import java.io.FileNotFoundException;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.LongSummaryStatistics;
import java.util.Map;
import java.util.Optional;
import java.util.stream.Collectors;

import aoc2020.AocBaseRunner;
import aoc2020.AocPuzzleInfo;
import aoc2020.AocResult;
import aoc2020.IAocPuzzle;
import aoc2020.InputUtils;

public class Day10 implements IAocPuzzle<List<Long>, Long, Long> {

    @Override
    public AocPuzzleInfo getInfo() {
        return new AocPuzzleInfo(2020, 10, "Adapter Array", true);
    }

    @Override
    public AocResult<Long, Long> getExpected() {
        return AocResult.of(2738L, 74049191673856L);
    }

    @Override
    public List<Long> parse(Optional<File> file) {
        List<Long> list = InputUtils.asLongList(file.get());
        Collections.sort(list);
        return list;
    }

    @Override
    public Long part1(List<Long> input) {
        long d1 = 0L;
        long d3 = 1L; // include last 3-diff to device
        long joltage = 0L;

        for (int i = 0; i < input.size(); i++) {
            long a = input.get(i);
            long diff = a - joltage;

            if (diff == 3L)
                d3++;
            else if (diff == 1L)
                d1++;
            else
                throw new RuntimeException();

            joltage += diff;
        }
        return d1 * d3;
    }

    @Override
    public Long part2(List<Long> input) {
        LongSummaryStatistics stats = input.stream()
                .collect(Collectors.summarizingLong(n -> n));
        input.add(stats.getMax() + 3); // add our device

        // Cache is a map from indexes in the (sorted) input list to the number
        // of ways it can be reached.
        Map<Long, Long> cache = new HashMap<>();
        cache.put(0L, 1L);

        // For each adapter, add the number of ways the previous three adapters
        // can be reached.
        for (long adapter : input) {
            long n = 0L;
            for (long x = 1; x <= 3; x++) {
                n += cache.getOrDefault(adapter - x, 0L);
            }
            cache.put(adapter, n);
        }

        return cache.values().stream()
                .collect(Collectors.summarizingLong(n -> n)).getMax();
    }

    public static void main(String[] args) throws FileNotFoundException {
        AocBaseRunner.run(new Day10());
    }

}
