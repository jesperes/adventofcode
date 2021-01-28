package aoc2017;

import java.io.File;
import java.io.IOException;
import java.util.Optional;
import java.util.stream.IntStream;

import common2.AocBaseRunner;
import common2.AocPuzzleInfo;
import common2.AocResult;
import common2.IAocIntPuzzle;

public class Day14 implements IAocIntPuzzle<String> {

    String input2 = "hfdlxzhv";

    public int[] knotHashDense(String str) {
        return new Day10().knotHashDense(str);
    }

    /**
     * Returns the knot hash as a binary string
     *
     * @param str
     * @return
     */
    public String knotHashAsBinaryString(String str) {
        StringBuilder builder = new StringBuilder();
        for (int n : knotHashDense(str)) {
            byte b = (byte) n;
            builder.append(byteToBinaryString(b));
        }
        return builder.toString();
    }

    private String byteToBinaryString(byte n) {
        StringBuilder b = new StringBuilder();

        for (int i = 7; i >= 0; i--) {
            if (((1 << i) & n) != 0) {
                b.append("1");
            } else {
                b.append("0");
            }
        }

        return b.toString();
    }

    /**
     * Compute the total number of ones in the binary representations of
     * knotHash(input + "-" + n) where n is 0 .. 127.
     *
     * @param input
     * @return
     */
    private int computeNumberOfOnes(String input) {
        return (int) IntStream.range(0, 128)
                .mapToObj(n -> input + "-" + String.valueOf(n))
                .flatMapToInt(s -> knotHashAsBinaryString(s).chars())
                .filter(n -> n == '1').count();
    }

    /**
     * Return the disk array for the given input string as an array of int
     * arrays.
     *
     * @param input
     * @return Array of int arrays, where -1 is "uncolored", and 0 is "empty".
     */
    private int[][] createColorGrid(String input) {
        return IntStream.range(0, 128)
                .mapToObj(n -> input + "-" + String.valueOf(n))
                .map(s -> knotHashAsBinaryString(s))
                .map(s -> s.chars().map(n -> n == '1' ? -1 : 0).toArray())
                .toArray(n -> new int[n][]);
    }

    /**
     * Fill a region starting at the given (i,j) coordinates using the specified
     * color.
     *
     * @param grid
     * @param i
     * @param j
     * @param color
     * @return true if the given coordinates were fill
     */
    private void fill(int[][] grid, int i, int j, int color) {
        // If coordinates are out of bounds, return.
        if (i < 0 || i >= grid.length || j < 0 || j >= grid[i].length)
            return;

        // We only care about uncolored grids.
        if (grid[i][j] != -1)
            return;

        grid[i][j] = color;
        fill(grid, i - 1, j, color);
        fill(grid, i, j - 1, color);
        fill(grid, i + 1, j, color);
        fill(grid, i, j + 1, color);
    }

    /**
     * Assign colors to continuous regions.
     *
     * @param grid
     * @return The number of colors/regions used.
     */
    private int assignColors(int[][] grid) {
        int color = 1;
        int numRegions = 0;
        for (int i = 0; i < grid.length; i++) {
            for (int j = 0; j < grid[i].length; j++) {
                if (grid[i][j] == -1) {
                    fill(grid, i, j, color++);
                    numRegions++;
                }
            }
        }
        return numRegions;
    }

    public void printGrid(int[][] grid) {

        for (int[] row : grid) {
            for (int color : row) {
                if (color > 0)
                    System.out.format("%d", color % 10);
                else
                    System.out.print(".");
            }

            System.out.println();
        }
    }

    @Override
    public AocPuzzleInfo getInfo() {
        return new AocPuzzleInfo(2017, 14, "Disk Defragmentation", false);
    }

    @Override
    public AocResult<Integer, Integer> getExpected() {
        return AocResult.of(8230, 1103);
    }

    @Override
    public String parse(Optional<File> file) throws IOException {
        return "hfdlxzhv";
    }

    @Override
    public Integer part1(String input) {
        return computeNumberOfOnes(input);
    }

    @Override
    public Integer part2(String input) {
        return assignColors(createColorGrid(input));
    }

    public static void main(String[] args) {
        AocBaseRunner.run(new Day14());
    }
}
