package aoc2015;

import static org.junit.Assert.assertEquals;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.nio.file.Files;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

import org.junit.Test;

import aoc2015.Day06.Instr;
import common2.AocPuzzleInfo;
import common2.AocResult;
import common2.IAocIntPuzzle;

public class Day06 implements IAocIntPuzzle<List<Instr>> {

    enum Mode {
        Toggle, TurnOn, TurnOff;
    }

    record Coord(int x, int y) {
    }

    record Instr(Mode mode, Coord from, Coord to) {
        static Instr parseString(String line) {
            String[] elems = line.split("[ ,]");
            Mode mode;
            int i;
            if (elems[0].equals("toggle")) {
                mode = Mode.Toggle;
                i = 1;
            } else if (elems[1].equals("on")) {
                mode = Mode.TurnOn;
                i = 2;
            } else {
                mode = Mode.TurnOff;
                i = 2;
            }

            return new Instr(mode,
                    new Coord(Integer.valueOf(elems[i]),
                            Integer.valueOf(elems[i + 1])),
                    new Coord(Integer.valueOf(elems[i + 3]),
                            Integer.valueOf(elems[i + 4])));
        }
    }

    @Test
    public void testDay06() throws IOException {
        try (BufferedReader reader = new BufferedReader(
                new FileReader("inputs/2015/day06.txt"))) {

            boolean grid1[][] = new boolean[1000][1000];
            int grid2[][] = new int[1000][1000];

            for (int x = 0; x <= 999; x++) {
                for (int y = 0; y <= 999; y++) {
                    grid1[x][y] = false;
                    grid2[x][y] = 0;
                }
            }

            String line;
            while ((line = reader.readLine()) != null) {
                String[] elems = line.split("[ ,]");
                Mode mode;
                int i;
                if (elems[0].equals("toggle")) {
                    mode = Mode.Toggle;
                    i = 1;
                } else if (elems[1].equals("on")) {
                    mode = Mode.TurnOn;
                    i = 2;
                } else {
                    mode = Mode.TurnOff;
                    i = 2;
                }

                int x1 = Integer.valueOf(elems[i]);
                int y1 = Integer.valueOf(elems[i + 1]);
                int x2 = Integer.valueOf(elems[i + 3]);
                int y2 = Integer.valueOf(elems[i + 4]);

                for (int x = x1; x <= x2; x++) {
                    for (int y = y1; y <= y2; y++) {
                        switch (mode) {
                        case Toggle:
                            grid1[x][y] = !grid1[x][y];
                            grid2[x][y] = grid2[x][y] + 2;
                            break;
                        case TurnOn:
                            grid1[x][y] = true;
                            grid2[x][y]++;
                            break;
                        case TurnOff:
                            grid1[x][y] = false;
                            grid2[x][y] = Math.max(0, grid2[x][y] - 1);
                            break;
                        }
                    }
                }
            }

            int numLit = 0;
            int totalBrightness = 0;

            for (int x = 0; x <= 999; x++) {
                for (int y = 0; y <= 999; y++) {
                    numLit += grid1[x][y] ? 1 : 0;
                    totalBrightness += grid2[x][y];
                }
            }

            assertEquals(543903, numLit);
            assertEquals(14687245, totalBrightness);
        }
    }

    @Override
    public AocPuzzleInfo getInfo() {
        return new AocPuzzleInfo(2015, 6, "Probably a Fire Hazard", true);
    }

    @Override
    public AocResult<Integer, Integer> getExpected() {
        return AocResult.of(543903, 14687245);
    }

    @Override
    public List<Instr> parse(Optional<File> file) {
        try {
            return Files.lines(file.get().toPath()).map(Instr::parseString)
                    .collect(Collectors.toList());
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    @Override
    public Integer part1(List<Instr> input) {
        int grid[][] = new int[1000][1000];

        for (Instr instr : input) {
            switch (instr.mode) {
            case Toggle:
                for (int x = instr.from.x; x <= instr.to.x; x++) {
                    for (int y = instr.from.y; y <= instr.to.y; y++) {
                        grid[x][y] = 1 - grid[x][y];
                    }
                }
                break;
            case TurnOff:
                for (int x = instr.from.x; x <= instr.to.x; x++) {
                    for (int y = instr.from.y; y <= instr.to.y; y++) {
                        grid[x][y] = 0;
                    }
                }
                break;
            case TurnOn:
                for (int x = instr.from.x; x <= instr.to.x; x++) {
                    for (int y = instr.from.y; y <= instr.to.y; y++) {
                        grid[x][y] = 1;
                    }
                }
            }
        }
        int count = 0;
        for (int x = 0; x <= 999; x++) {
            for (int y = 0; y <= 999; y++) {
                count += grid[x][y];
            }
        }
        return count;
    }

    @Override
    public Integer part2(List<Instr> input) {
        int grid[][] = new int[1000][1000];

        for (Instr instr : input) {
            switch (instr.mode) {
            case Toggle:
                for (int x = instr.from.x; x <= instr.to.x; x++) {
                    for (int y = instr.from.y; y <= instr.to.y; y++) {
                        grid[x][y] += 2;
                    }
                }
                break;
            case TurnOff:
                for (int x = instr.from.x; x <= instr.to.x; x++) {
                    for (int y = instr.from.y; y <= instr.to.y; y++) {
                        grid[x][y]--;
                        if (grid[x][y] < 0) {
                            grid[x][y] = 0;
                        }
                    }
                }
                break;
            case TurnOn:
                for (int x = instr.from.x; x <= instr.to.x; x++) {
                    for (int y = instr.from.y; y <= instr.to.y; y++) {
                        grid[x][y]++;
                    }
                }
            }
        }
        int count = 0;
        for (int x = 0; x <= 999; x++) {
            for (int y = 0; y <= 999; y++) {
                count += grid[x][y];
            }
        }
        return count;
    }
}
