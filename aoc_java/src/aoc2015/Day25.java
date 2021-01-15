package aoc2015;

import java.io.File;
import java.io.IOException;
import java.util.Optional;

import aoc2015.Day25.RowCol;
import common2.AocBaseRunner;
import common2.AocPuzzleInfo;
import common2.AocResult;
import common2.IAocPuzzle;

public class Day25 implements IAocPuzzle<RowCol, Long, Void> {

    record RowCol(int row, int col) {
    }

    @Override
    public AocPuzzleInfo getInfo() {
        return new AocPuzzleInfo(2015, 25, "Let It Snow", false);
    }

    @Override
    public AocResult<Long, Void> getExpected() {
        return AocResult.of(8997277L, null);
    }

    @Override
    public RowCol parse(Optional<File> file) throws IOException {
        return new RowCol(3010, 3019);
    }

    @Override
    public Long part1(RowCol input) {
        int row = 1;
        int col = 1;
        long value = 20151125L;

        while (true) {
            value = (value * 252533L) % 33554393;

            if (row == 1) {
                row = col + 1;
                col = 1;
            } else {
                row--;
                col++;
            }

            if (row == input.row && col == input.col)
                return value;
        }
    }

    @Override
    public Void part2(RowCol input) {
        return null;
    }

    public static void main(String[] args) {
        AocBaseRunner.run(new Day25());
    }
}
