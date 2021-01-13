package aoc2015;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

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

    @Override
    public AocPuzzleInfo getInfo() {
        return new AocPuzzleInfo(2015, 6, "Probably a Fire Hazard", true);
    }

    @Override
    public AocResult<Integer, Integer> getExpected() {
        return AocResult.of(543903, 14687245);
    }

    @Override
    public List<Instr> parse(Optional<File> file) throws IOException {
        return Files.lines(file.get().toPath()).map(Instr::parseString)
                .collect(Collectors.toList());
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
