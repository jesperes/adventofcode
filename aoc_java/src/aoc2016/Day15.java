package aoc2016;

import java.io.File;
import java.io.IOException;
import java.util.Optional;

import common2.AocBaseRunner;
import common2.AocPuzzleInfo;
import common2.AocResult;
import common2.IAocIntPuzzle;

public class Day15 implements IAocIntPuzzle<Void> {

    @Override
    public AocPuzzleInfo getInfo() {
        return new AocPuzzleInfo(2016, 15, "Timing is Everything", false);
    }

    @Override
    public AocResult<Integer, Integer> getExpected() {
        return AocResult.of(400589, 3045959);
    }

    @Override
    public Void parse(Optional<File> file) throws IOException {
        // Parsing the input file is just a waste of time.
        return null;
    }

    @Override
    public Integer part1(Void input) {
        int[] a = new int[] { 15, 2, 4, 2, 2, 0 };
        int[] n = new int[] { 17, 3, 19, 13, 7, 5 };
        return solve(a, n);
    }

    @Override
    public Integer part2(Void input) {
        int[] a = new int[] { 15, 2, 4, 2, 2, 0, 0 };
        int[] n = new int[] { 17, 3, 19, 13, 7, 5, 11 };
        return solve(a, n);
    }

    /*
     * Weirdly enough, the CRT implementation at Rosetta-code seems to generate
     * incorrect results, and I'm too lazy to figure out why. Also, this is
     * quite fast given the "smallish" input.
     * https://rosettacode.org/wiki/Chinese_remainder_theorem#Java
     */
    int solve(int[] a, int[] n) {
        int t = 1;
        while (true) {
            int incr = 1;

            for (int i = 0; i < a.length; i++) {
                if ((t + a[i] + (i + 1)) % n[i] == 0) {
                    incr *= n[i];
                    if (i == a.length - 1) {
                        return t;
                    }
                } else {
                    break;
                }
            }

            t += incr;
        }
    }

    public static void main(String[] args) {
        AocBaseRunner.run(new Day15());
    }
}
