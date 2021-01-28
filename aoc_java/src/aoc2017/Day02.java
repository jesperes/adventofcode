package aoc2017;

import static org.junit.Assert.assertEquals;

import java.io.File;
import java.io.IOException;
import java.util.Optional;

import common2.AocBaseRunner;
import common2.AocPuzzleInfo;
import common2.AocResult;
import common2.IAocIntPuzzle;
import common2.InputUtils;

public class Day02 implements IAocIntPuzzle<int[][]> {
    static final int SIZE = 16;

    @Override
    public AocPuzzleInfo getInfo() {
        return new AocPuzzleInfo(2017, 2, "Corruption Checksum", true);
    }

    @Override
    public AocResult<Integer, Integer> getExpected() {
        return AocResult.of(37923, 263);
    }

    @Override
    public int[][] parse(Optional<File> file) throws IOException {
        int[][] grid = new int[SIZE][SIZE];

        int row = 0;
        for (String line : InputUtils.asStringList(file.get())) {
            String[] elems = line.split("\\s+");
            assertEquals(SIZE, elems.length);
            int col = 0;
            for (String s : elems) {
                int n = Integer.parseInt(s);
                grid[row][col] = n;
                col++;
            }
            row++;
        }
        return grid;
    }

    @Override
    public Integer part1(int[][] grid) {
        int checksum = 0;
        for (int row = 0; row < SIZE; row++) {
            int min = Integer.MAX_VALUE;
            int max = Integer.MIN_VALUE;

            for (int col = 0; col < SIZE; col++) {
                min = Math.min(min, grid[row][col]);
                max = Math.max(max, grid[row][col]);
            }

            checksum += (max - min);
        }

        return checksum;
    }

    @Override
    public Integer part2(int[][] grid) {
        int checksum = 0;
        for (int row = 0; row < SIZE; row++) {
            boolean found = false;
            for (int col1 = 0; col1 < SIZE && !found; col1++) {
                for (int col2 = 0; col2 < SIZE && !found; col2++) {
                    if (col1 == col2)
                        continue;

                    int a = grid[row][col1];
                    int b = grid[row][col2];

                    if (a > b && (a % b == 0)) {
                        checksum += (a / b);
                        found = true;
                    }
                }
            }
        }

        return checksum;
    }

    public static void main(String[] args) {
        AocBaseRunner.run(new Day02());
    }
}
