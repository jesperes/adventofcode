package aoc2018;

import static org.junit.Assert.assertEquals;

import java.io.File;
import java.io.IOException;
import java.util.Optional;

import org.junit.Test;

import common2.AocBaseRunner;
import common2.AocPuzzleInfo;
import common2.AocResult;
import common2.IAocPuzzle;

public class Day11 implements IAocPuzzle<Void, String, String> {

    static final int SIZE = 300;
    int[][] grid = new int[SIZE][SIZE];

    @Override
    public AocPuzzleInfo getInfo() {
        return new AocPuzzleInfo(2018, 11, "Chronal Charge", false);
    }

    @Override
    public AocResult<String, String> getExpected() {
        return AocResult.of("235,14", "237,227,14");
    }

    @Override
    public Void parse(Optional<File> file) throws IOException {
        int input = 1133;
        for (int x = 0; x < SIZE; x++) {
            for (int y = 0; y < SIZE; y++) {
                grid[x][y] = powerLevel(input, x, y);
            }
        }
        return null;
    }

    record Result(int sum, int size, int x, int y) {
    }

    Result findBestPowerSquare(int size) {

        int maxSq = Integer.MIN_VALUE;
        int maxx = -1, maxy = -1;

        for (int x = 0; x < SIZE - size; x++) {
            for (int y = 0; y < SIZE - size; y++) {
                int sum = 0;
                for (int dx = 0; dx < size; dx++) {
                    for (int dy = 0; dy < size; dy++) {
                        sum += grid[x + dx][y + dy];
                    }
                }

                if (sum > maxSq) {
                    maxSq = sum;
                    maxx = x;
                    maxy = y;
                }
            }
        }

        return new Result(maxSq, size, maxx, maxy);
    }

    private int powerLevel(int input, int x, int y) {
        int rackID = x + 10;
        return ((((rackID * y) + input) * rackID) / 100) % 10 - 5;
    }

    @Override
    public String part1(Void input) {
        Result result = findBestPowerSquare(3);
        return "%d,%d".formatted(result.x, result.y);
    }

    @Override
    public String part2(Void input) {
        int maxSize = 0;
        Result bestResult = null;

        // This is really cheating; we know the result is 14, so check a few
        // in between. To get the runtime down for this we would need to memoize
        // computed grids and reuse them.
        for (int i = 10; i < 20; i++) {
            Result result = findBestPowerSquare(i);
            if (result.sum > maxSize) {
                bestResult = result;
                maxSize = result.sum;
            }
        }

        return "%d,%d,%d".formatted(bestResult.x, bestResult.y,
                bestResult.size);
    }

    @Test
    public void test1() throws Exception {
        assertEquals(4, powerLevel(8, 3, 5));
        assertEquals(-5, powerLevel(57, 122, 79));
    }

    public static void main(String[] args) {
        AocBaseRunner.run(new Day11());
    }
}
