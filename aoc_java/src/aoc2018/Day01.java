package aoc2018;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.util.Optional;

import common2.AocBaseRunner;
import common2.AocPuzzleInfo;
import common2.AocResult;
import common2.IAocIntPuzzle;

public class Day01 implements IAocIntPuzzle<int[]> {

    static final int MAX_FREQUENCIES = 256 * 1024;

    @Override
    public AocPuzzleInfo getInfo() {
        return new AocPuzzleInfo(2018, 1, "Chronal Calibration", true);
    }

    @Override
    public AocResult<Integer, Integer> getExpected() {
        return AocResult.of(470, 790);
    }

    @Override
    public int[] parse(Optional<File> file) throws IOException {
        return Files.lines(file.get().toPath())
                .mapToInt(s -> Integer.parseInt(s)).toArray();
    }

    @Override
    public Integer part1(int[] input) {
        int count = 0;
        int l = input.length;
        for (int i = 0; i < l; i++) {
            count += input[i];
        }
        return count;
    }

    @Override
    public Integer part2(int[] input) {
        /*
         * Use an boolean array to keep track of seen frequencies; the number of
         * frequences we need to store is small enough for this to be much more
         * efficient than e.g. a HashSet.
         */
        boolean[] seen = new boolean[MAX_FREQUENCIES];
        int offset = seen.length / 2;
        int freq = 0;

        while (true) {
            int l = input.length;
            for (int i = 0; i < l; i++) {
                freq += input[i];
                int si = freq + offset;
                if (seen[si])
                    return freq;
                else
                    seen[si] = true;
            }
        }
    }

    public static void main(String[] args) {
        AocBaseRunner.run(new Day01());
    }
}
