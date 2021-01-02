package aoc2020.solutions;

import static org.junit.Assert.assertEquals;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.nio.file.Files;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

import org.junit.Test;

import aoc2020.AocBaseRunner;
import aoc2020.AocPuzzleInfo;
import aoc2020.AocResult;
import aoc2020.IAocPuzzle;
import aoc2020.solutions.Day12.NavInstr;

public class Day12 implements IAocPuzzle<List<NavInstr>, Long, Long> {
    enum Action {
        N, S, E, W, L, R, F
    }

    record Coord(long x, long y) {
    }

    record NavInstr(Action action, int value) {
    }

    @Override
    public AocPuzzleInfo getInfo() {
        return new AocPuzzleInfo(2020, 12, "Rain Risk", true);
    }

    @Override
    public AocResult<Long, Long> getExpected() {
        return AocResult.of(845L, 27016L);
    }

    @Override
    public List<NavInstr> parse(Optional<File> file) {
        try {
            return Files.lines(file.get().toPath())
                    .map(line -> new NavInstr(
                            Action.valueOf(line.substring(0, 1)),
                            Integer.parseInt(line.substring(1))))
                    .collect(Collectors.toList());
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    @Override
    public Long part1(List<NavInstr> input) {
        long x = 0;
        long y = 0;
        int dir = 90;
        for (NavInstr instr : input) {
            switch (instr.action) {
            case N:
                y -= instr.value;
                break;
            case E:
                x += instr.value;
                break;
            case S:
                y += instr.value;
                break;
            case W:
                x -= instr.value;
                break;
            case L:
                dir = left(dir, instr.value);
                break;
            case R:
                dir = right(dir, instr.value);
                break;
            case F:
                switch (dir) {
                case 0:
                    y -= instr.value;
                    break;
                case 90:
                    x += instr.value;
                    break;
                case 180:
                    y += instr.value;
                    break;
                case 270:
                    x -= instr.value;
                    break;
                default:
                    throw new RuntimeException();
                }
                break;
            default:
                throw new RuntimeException();
            }
        }
        return Math.abs(x) + Math.abs(y);
    }

    @Override
    public Long part2(List<NavInstr> input) {
        long x = 0;
        long y = 0;
        long wpx = 10;
        long wpy = -1;

        for (NavInstr instr : input) {
            switch (instr.action) {
            case N:
                wpy -= instr.value;
                break;
            case E:
                wpx += instr.value;
                break;
            case S:
                wpy += instr.value;
                break;
            case W:
                wpx -= instr.value;
                break;
            case L: {
                Coord wp = wpLeft(new Coord(wpx, wpy), instr.value);
                wpx = wp.x;
                wpy = wp.y;
                break;
            }
            case R: {
                Coord wp = wpRight(new Coord(wpx, wpy), instr.value);
                wpx = wp.x;
                wpy = wp.y;
                break;
            }
            case F: {
                x += wpx * instr.value;
                y += wpy * instr.value;
                break;
            }
            default:
                throw new RuntimeException();
            }
        }
        return Math.abs(x) + Math.abs(y);
    }

    private Coord wpRight(Coord c, int value) {
        if (value == 0) {
            return c;
        } else {
            return wpRight(new Coord(-c.y, c.x), value - 90);
        }
    }

    private Coord wpLeft(Coord c, int value) {
        if (value == 0) {
            return c;
        } else {
            return wpLeft(new Coord(c.y, -c.x), value - 90);
        }
    }

    private int left(int dir, int val) {
        return ((dir + 360) - val) % 360;
    }

    private int right(int dir, int val) {
        return (dir + val) % 360;
    }

    public static void main(String[] args) throws FileNotFoundException {
        AocBaseRunner.run(new Day12());
    }

    @Test
    public void testP1() throws Exception {
        long n = part1(List.of(//
                new NavInstr(Action.F, 10), //
                new NavInstr(Action.N, 3), //
                new NavInstr(Action.F, 7), //
                new NavInstr(Action.R, 90), //
                new NavInstr(Action.F, 11)));

        assertEquals(25L, n);
    }
}
