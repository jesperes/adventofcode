package aoc2020.solutions;

import java.io.File;
import java.io.FileNotFoundException;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

import aoc2020.AocBaseRunner;
import aoc2020.AocPuzzleInfo;
import aoc2020.AocResult;
import aoc2020.IAocPuzzle;
import aoc2020.InputUtils;

public class Day09 implements IAocPuzzle<List<Long>, Long, Long> {

    @Override
    public AocPuzzleInfo getInfo() {
        return new AocPuzzleInfo(2020, 9, "Encoding Error", true);
    }

    @Override
    public AocResult<Long, Long> getExpected() {
        return AocResult.of(138879426L, 23761694L);
    }

    @Override
    public List<Long> parse(Optional<File> file) {
        return InputUtils.asLongList(file.get());
    }

    @Override
    public Long part1(List<Long> input) {
        final int n = 25;

        for (int i = n; i < input.size(); i++) {
            long num = input.get(i);

            boolean isValid = false;
            for (int j = i - n; j < i && !isValid; j++) {
                long a = input.get(j);

                for (int k = j + 1; k < i && !isValid; k++) {
                    long b = input.get(k);

                    if (a + b == num) {
                        isValid = true;
                        break;
                    }
                }
            }
            if (!isValid)
                return num;
        }
        throw new RuntimeException();
    }

    @Override
    public Long part2(List<Long> input) {
        var expected = getExpected();
        var p1 = expected.p1().get();

        for (int i = 2; i <= input.size(); i++) {
            // i is the size of the range; first we try all possible ranges
            // of length 'i'.

            for (int j = 0; j < input.size() - i; j++) {
                var range = input.subList(j, j + i);
                var s = sum(range);
                if (s == p1) {
                    var stats = range.stream()
                            .collect(Collectors.summarizingLong(n -> n));
                    return stats.getMin() + stats.getMax();
                }
            }
        }
        throw new RuntimeException();
    }

    private long sum(List<Long> list) {
        long n = 0;
        for (long x : list) {
            n += x;
        }
        return n;
    }

    public static void main(String[] args) throws FileNotFoundException {
        AocBaseRunner.run(new Day09());
    }

}
