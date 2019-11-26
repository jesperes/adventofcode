package aoc2015;

import static org.junit.Assert.assertEquals;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;

import org.junit.Test;

public class Day18 {

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

    int runIters(boolean cornerLightsStuck) throws IOException {

        char grid1[][] = new char[100][100];
        char grid2[][] = new char[100][100];

        try (BufferedReader reader = new BufferedReader(
                new FileReader("inputs/2015/day18.txt"))) {
            String line;
            int y = 0;
            while ((line = reader.readLine()) != null) {
                for (int x = 0; x < 100; x++) {
                    grid1[y][x] = line.charAt(x);
                }
                y++;
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
            char[][] tmp = grid2;
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

    @Test
    public void testDay18() throws Exception {
        assertEquals(768, runIters(false));
        assertEquals(781, runIters(true));
    }
}
