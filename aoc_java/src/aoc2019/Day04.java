package aoc2019;

import java.io.File;
import java.io.IOException;
import java.util.Optional;

import aoc2019.Day04.Range;
import common2.AocBaseRunner;
import common2.AocPuzzleInfo;
import common2.AocResult;
import common2.IAocIntPuzzle;

public class Day04 implements IAocIntPuzzle<Range> {

    record Range(int lower, int upper) {
    }

    @Override
    public AocPuzzleInfo getInfo() {
        return new AocPuzzleInfo(2019, 4, "Secure Container", false);
    }

    @Override
    public AocResult<Integer, Integer> getExpected() {
        return AocResult.of(454, 288);
    }

    @Override
    public Range parse(Optional<File> file) throws IOException {
        return new Range(402328, 864247);
    }

    @Override
    public Integer part1(Range input) {
        int n = 0;
        for (int i = input.lower; i <= input.upper; i++) {
            char[] s = String.valueOf(i).toCharArray();
            char a = s[0];
            char b = s[1];
            char c = s[2];
            char d = s[3];
            char e = s[4];
            char f = s[5];

            if (a > b || b > c || c > d || d > e || e > f)
                continue;

            if (a == b || b == c || c == d || d == e || e == f) {
                n++;
            }
        }

        return n;
    }

    @Override
    public Integer part2(Range input) {
        int n = 0;
        for (int i = input.lower; i <= input.upper; i++) {
            char[] s = String.valueOf(i).toCharArray();
            char a = s[0];
            char b = s[1];
            char c = s[2];
            char d = s[3];
            char e = s[4];
            char f = s[5];

            if (a > b || b > c || c > d || d > e || e > f)
                continue;

            if ((a == b || b == c || c == d || d == e || e == f)
                    && ((a == b && b != c) || (a != b && b == c && c != d)
                            || (b != c && c == d && d != e)
                            || (c != d && d == e && e != f)
                            || (d != e && e == f)))
                n++;
        }
        return n;
    }

    public static void main(String[] args) {
        AocBaseRunner.run(new Day04());
    }
}
