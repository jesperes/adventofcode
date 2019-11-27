package aoc2016;

import java.io.BufferedReader;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;

public class Day17 {
    static class Range implements Comparable<Range> {
        long lower;
        long upper;

        public Range(long lower, long upper) {
            super();
            this.lower = lower;
            this.upper = upper;
        }

        @Override
        public int compareTo(Range o) {
            return Long.compare(lower, o.lower);
        }

        public boolean inRange(long n) {
            return n >= lower && n <= upper;
        }

        @Override
        public String toString() {
            return String.format("[%d-%d]", lower, upper);
        }

    }

    static long portMin = 0;
    static long portMax = 4294967295L;

    public static void main(String[] args)
            throws FileNotFoundException, IOException {
        try (BufferedReader reader = new BufferedReader(
                new FileReader("input.txt"))) {
            List<Range> ranges = reader.lines().map(l -> {
                String[] elems = l.split("-");
                Long lower = Long.valueOf(elems[0]);
                Long upper = Long.valueOf(elems[1]);
                return new Range(lower, upper);
            }).collect(Collectors.toList());

            // Sort ranges on lower bound
            Collections.sort(ranges);

            List<Range> testRanges = Arrays.asList(new Range(5, 8),
                    new Range(0, 2), new Range(4, 7));
            Collections.sort(testRanges);

            System.out.println(findFirstNotIn(ranges));
            System.out.println(countNumbersNotInRange(ranges));
        }
    }

    private static Range inRanges(List<Range> ranges, long n) {
        for (Range range : ranges) {
            if (range.inRange(n))
                return range;
        }

        return null;
    }

    private static long findFirstNotIn(List<Range> ranges) {
        for (long n = portMin; n < portMax;) {
            Range range = inRanges(ranges, n);
            if (range != null) {
                n = range.upper + 1;
            } else {
                return n;
            }
        }

        return -1;
    }

    private static long countNumbersNotInRange(List<Range> ranges) {
        long sum = 0;
        long current = 0;

        for (Range range : ranges) {
            // add numbers before this range
            sum += Math.max(0, range.lower - current);

            // move to end of this range, but never move back
            current = Math.max(current, range.upper + 1);
        }

        // finish up to portMax
        return sum + portMax - current + 1;
    }
}
