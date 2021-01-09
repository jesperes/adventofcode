package aoc2020;

import static org.junit.Assert.assertEquals;

import java.io.File;
import java.io.FileNotFoundException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

import org.junit.Test;

import aoc2020.Day13.Departures;
import common2.AocBaseRunner;
import common2.AocPuzzleInfo;
import common2.AocResult;
import common2.IAocPuzzle;
import common2.InputUtils;

public class Day13 implements IAocPuzzle<Departures, Long, Long> {

    record Departures(long timestamp, List<Integer> ids) {

    }

    /**
     * Comparable pair of values.
     * 
     * @author jesperes
     *
     * @param <T1>
     * @param <T2>
     */
    record Pair<T1 extends Comparable<T1>, T2 extends Comparable<T2>> (T1 first,
            T2 second) implements Comparable<Pair<T1, T2>> {
        @Override
        public int compareTo(Pair<T1, T2> o) {
            int cmp = first.compareTo(o.first());
            if (cmp != 0) {
                return cmp;
            } else {
                return second.compareTo(o.second());
            }
        }

    }

    @Override
    public AocPuzzleInfo getInfo() {
        return new AocPuzzleInfo(2020, 13, "Shuttle Search", true);
    }

    @Override
    public AocResult<Long, Long> getExpected() {
        return AocResult.of(333L, 690123192779524L);
    }

    @Override
    public Departures parse(Optional<File> file) {
        List<String> list = InputUtils.asStringList(file.get());
        return new Departures(Long.parseLong(list.get(0)),
                Arrays.stream(list.get(1).split(","))
                        .map(s -> s.equals("x") ? -1 : Integer.parseInt(s))
                        .collect(Collectors.toList()));
    }

    @Override
    public Long part1(Departures input) {
        return input.ids.stream().filter(id -> id >= 0)
                .map(id -> new Pair<>(id - (input.timestamp % id), id)).sorted()
                .findFirst().map(pair -> pair.first * pair.second).get();
    }

    @Override
    public Long part2(Departures input) {
        List<Long> nlist = new ArrayList<>();
        List<Long> alist = new ArrayList<>();

        for (int i = 0; i < input.ids.size(); i++) {
            long modulo = input.ids.get(i);
            if (modulo == -1)
                continue;

            long residue = modulo - i;
            nlist.add(modulo);
            alist.add(residue);

        }

        long[] n = nlist.stream().mapToLong(x -> x).toArray();
        long[] a = alist.stream().mapToLong(x -> x).toArray();
        return chineseRemainder(n, a);
    }

    // https://rosettacode.org/wiki/Chinese_remainder_theorem#Java
    private static long chineseRemainder(long[] n, long[] a) {
        long prod = Arrays.stream(n).reduce(1, (i, j) -> i * j);

        long p, sm = 0;
        for (int i = 0; i < n.length; i++) {
            p = prod / n[i];
            sm += a[i] * mulInv(p, n[i]) * p;
        }

        return sm % prod;
    }

    private static long mulInv(long a, long b) {
        long b0 = b;
        long x0 = 0;
        long x1 = 1;

        if (b == 1)
            return 1;

        while (a > 1) {
            long q = a / b;
            long amb = a % b;
            a = b;
            b = amb;
            long xqx = x1 - q * x0;
            x1 = x0;
            x0 = xqx;
        }

        if (x1 < 0)
            x1 += b0;

        return x1;
    }

    public static void main(String[] args) throws FileNotFoundException {
        AocBaseRunner.run(new Day13());
    }

    @Test
    public void testCRT() throws Exception {
        assertEquals(23, chineseRemainder(//
                new long[] { 3L, 5L, 7L }, // n (moduli)
                new long[] { 2L, 3L, 2L } // a (residues)
        ));
    }
}
