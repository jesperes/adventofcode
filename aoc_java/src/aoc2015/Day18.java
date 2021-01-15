package aoc2015;

import java.io.File;
import java.io.IOException;
import java.util.Optional;

import common2.AocBaseRunner;
import common2.AocPuzzleInfo;
import common2.AocResult;
import common2.IAocIntPuzzle;
import common2.InputUtils;

public class Day18 implements IAocIntPuzzle<char[][]> {

    boolean isCorner(int y, int x) {
        return ((y == 0 && x == 0) || (y == 0 && x == 99) || (y == 99 && x == 0)
                || (y == 99 && x == 99));
    }

    boolean isLit(char[][] grid, int y, int x, boolean cornerLightsStuck) {
        if (cornerLightsStuck && isCorner(y, x))
            return true;
        else {
            if (x >= 0 && x < grid.length && y >= 0 && y < grid.length) {
                return grid[y][x] == '#';
            } else {
                return false;
            }
        }
    }

    @Override
    public AocPuzzleInfo getInfo() {
        return new AocPuzzleInfo(2015, 18, "Like a GIF For Your Yard", true);
    }

    @Override
    public AocResult<Integer, Integer> getExpected() {
        return AocResult.of(768, 781);
    }

    @Override
    public char[][] parse(Optional<File> file) throws IOException {
        char grid[][] = new char[100][100];
        int y = 0;
        for (String line : InputUtils.asStringList(file.get())) {
            for (int x = 0; x < 100; x++) {
                grid[y][x] = line.charAt(x);
            }
            y++;
        }
        return grid;
    }

    private int runIters(char[][] grid, boolean cornerLightsStuck) {

        char grid1[][] = new char[100][100];
        char grid2[][] = new char[100][100];

        for (int y = 0; y < 100; y++) {
            for (int x = 0; x < 100; x++) {
                grid1[y][x] = grid[x][y];
            }
        }

        for (int i = 0; i < 100; i++) {
            for (int y = 0; y < 100; y++) {
                for (int x = 0; x < 100; x++) {
                    int nbrs = 0;

                    if (isLit(grid1, y - 1, x - 1, cornerLightsStuck))
                        nbrs++;
                    if (isLit(grid1, y - 1, x, cornerLightsStuck))
                        nbrs++;
                    if (isLit(grid1, y - 1, x + 1, cornerLightsStuck))
                        nbrs++;
                    if (isLit(grid1, y, x - 1, cornerLightsStuck))
                        nbrs++;
                    if (isLit(grid1, y, x + 1, cornerLightsStuck))
                        nbrs++;
                    if (isLit(grid1, y + 1, x - 1, cornerLightsStuck))
                        nbrs++;
                    if (isLit(grid1, y + 1, x, cornerLightsStuck))
                        nbrs++;
                    if (isLit(grid1, y + 1, x + 1, cornerLightsStuck))
                        nbrs++;

                    /*
                     * A light which is on stays on when 2 or 3 neighbors are
                     * on, and turns off otherwise. A light which is off turns
                     * on if exactly 3 neighbors are on, and stays off
                     * otherwise.
                     */
                    if (grid1[y][x] == '#') {
                        if (nbrs == 2 || nbrs == 3)
                            grid2[y][x] = '#';
                        else
                            grid2[y][x] = '.';
                    }
                    if (grid1[y][x] == '.') {
                        if (nbrs == 3)
                            grid2[y][x] = '#';
                        else
                            grid2[y][x] = '.';
                    }
                }
            }

            // swap the grids;
            var tmp = grid2;
            grid2 = grid1;
            grid1 = tmp;
        }

        int numLit = 0;
        for (int y = 0; y < 100; y++) {
            for (int x = 0; x < 100; x++) {
                if (isLit(grid1, y, x, cornerLightsStuck))
                    numLit++;
            }
        }

        return numLit;
    }

    @Override
    public Integer part1(char[][] input) {
        return runIters(input, false);
    }

    @Override
    public Integer part2(char[][] input) {
        return runIters(input, true);
    }

    public static void main(String[] args) {
        AocBaseRunner.run(new Day18());
    }
}
