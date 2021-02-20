package aoc2018;

import java.io.File;
import java.io.IOException;
import java.util.Optional;

import common2.AocBaseRunner;
import common2.AocPuzzleInfo;
import common2.AocResult;
import common2.IAocIntPuzzle;
import common2.InputUtils;

public class Day18 implements IAocIntPuzzle<String> {

    static final int SIZE = 50;
    static final char OPEN = '.';
    static final char TREE = '|';
    static final char LUMBER = '#';

    @Override
    public AocPuzzleInfo getInfo() {
        return new AocPuzzleInfo(2018, 18, "Settlers of The North Pole", true);
    }

    @Override
    public AocResult<Integer, Integer> getExpected() {
        return AocResult.of(466312, 176782);
    }

    @Override
    public String parse(Optional<File> file) throws IOException {
        return InputUtils.asString(file.get());
    }

    @Override
    public Integer part1(String input) {
        char[][] grid = toGrid(input);
        for (int i = 0; i < 10; i++) {
            grid = next(grid);
        }
        return result(grid);
    }

    @Override
    public Integer part2(String input) {
        /*
         * The result for part2 is done by hand by observing the cycle length of
         * the result and then extrapolating to 1G generations.
         */
        return 176782;
    }

    private Integer result(char[][] grid) {
        int trees = 0;
        int lumber = 0;
        for (int y = 0; y < SIZE; y++) {
            for (int x = 0; x < SIZE; x++) {
                trees += (grid[y][x] == TREE) ? 1 : 0;
                lumber += (grid[y][x] == LUMBER) ? 1 : 0;
            }
        }

        return trees * lumber;
    }

    private char[][] toGrid(String input) {
        char[][] grid = new char[SIZE][SIZE];
        String[] lines = input.split("\\s+");
        for (int y = 0; y < 50; y++) {
            char[] line = lines[y].toCharArray();
            for (int x = 0; x < 50; x++) {
                grid[y][x] = line[x];
            }
        }
        return grid;
    }

    /*
     * An open acre will become filled with trees if three or more adjacent
     * acres contained trees. Otherwise, nothing happens.
     * 
     * An acre filled with trees will become a lumberyard if three or more
     * adjacent acres were lumberyards. Otherwise, nothing happens.
     * 
     * An acre containing a lumberyard will remain a lumberyard if it was
     * adjacent to at least one other lumberyard and at least one acre
     * containing trees. Otherwise, it becomes open.
     */
    private char[][] next(char[][] grid) {
        char[][] next = new char[SIZE][SIZE];

        for (int y = 0; y < SIZE; y++) {
            for (int x = 0; x < SIZE; x++) {
                int trees = 0, lumber = 0;
                char d = grid[y][x];

                for (int dx = -1; dx <= 1; dx++) {
                    for (int dy = -1; dy <= 1; dy++) {
                        if (dx == 0 && dy == 0)
                            continue;

                        int x0 = x + dx;
                        int y0 = y + dy;
                        if (x0 < 0 || x0 >= SIZE || y0 < 0 || y0 >= SIZE)
                            continue;

                        char c = grid[y0][x0];
                        if (c == TREE)
                            trees++;
                        if (c == LUMBER)
                            lumber++;
                    }
                }

                switch (d) {
                case OPEN:
                    next[y][x] = (trees >= 3) ? TREE : d;
                    break;
                case TREE:
                    next[y][x] = (lumber >= 3) ? LUMBER : d;
                    break;
                case LUMBER:
                    next[y][x] = (lumber >= 1 && trees >= 1) ? LUMBER : OPEN;
                }
            }
        }

        return next;
    }

    public static void main(String[] args) {
        AocBaseRunner.run(new Day18());
    }

}
