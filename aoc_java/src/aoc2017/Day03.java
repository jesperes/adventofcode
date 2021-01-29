package aoc2017;

import static org.junit.Assert.assertFalse;

import java.io.File;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;
import java.util.Optional;

import common2.AocBaseRunner;
import common2.AocPuzzleInfo;
import common2.AocResult;
import common2.IAocIntPuzzle;

public class Day03 implements IAocIntPuzzle<Integer> {

    enum Dir {
        // @formatter:off
        n { @Override Dir turn() { return w; } },
        e { @Override Dir turn() { return n; } },
        s { @Override Dir turn() { return e; } },
        w { @Override Dir turn() { return s; } };
        // @formatter:on

        abstract Dir turn();
    }

    record Pos(int x, int y) {
        Pos fwd(Dir d) {
            // @formatter:off
            switch (d) {
            case e: return new Pos(x + 1, y);
            case n: return new Pos(x, y - 1);
            case s: return new Pos(x, y + 1);
            case w: return new Pos(x - 1, y);
            default: throw new RuntimeException();
            // @formatter:on
            }
        }

        Pos left(Dir d) {
            switch (d) {
            // @formatter:off
            case e: return new Pos(x, y - 1);
            case n: return new Pos(x - 1, y);
            case s: return new Pos(x + 1, y);
            case w: return new Pos(x, y + 1);
            default: throw new RuntimeException();
            // @formatter:on
            }
        }
    }

    @Override
    public AocPuzzleInfo getInfo() {
        return new AocPuzzleInfo(2017, 3, "Spiral Memory", false);
    }

    @Override
    public AocResult<Integer, Integer> getExpected() {
        return AocResult.of(326, 363010);
    }

    @Override
    public Integer parse(Optional<File> file) throws IOException {
        return 361527;
    }

    @Override
    public Integer part1(Integer input) {
        Map<Pos, Integer> grid = new HashMap<>();
        Dir d = Dir.e;
        Pos p = new Pos(0, 0);
        for (int i = 1; i < input; i++) {
            grid.put(p, i);
            var left = p.left(d);
            if (grid.containsKey(left)) {
                p = p.fwd(d);
            } else {
                p = left;
                d = d.turn();
            }
        }
        return Math.abs(p.x) + Math.abs(p.y);
    }

    @Override
    public Integer part2(Integer input) {
        Map<Pos, Integer> grid = new HashMap<>();
        Dir d = Dir.s;
        Pos p = new Pos(0, 0);
        grid.put(p, 1);
        while (true) {
            var left = p.left(d);

            if (grid.containsKey(left)) {
                p = p.fwd(d);
            } else {
                p = left;
                d = d.turn();
            }

            int adj = sumAdjacents(grid, p);
            if (adj > input)
                return adj;

            assertFalse(grid.containsKey(p));
            grid.put(p, adj);
        }

    }

    private Integer sumAdjacents(Map<Pos, Integer> grid, Pos p) {
        int sum = 0;
        for (int dx = -1; dx <= 1; dx++) {
            for (int dy = -1; dy <= 1; dy++) {
                if (dx == 0 && dy == 0)
                    continue;

                sum += grid.getOrDefault(new Pos(p.x + dx, p.y + dy), 0);
            }
        }
        return sum;
    }

    public static void main(String[] args) {
        AocBaseRunner.run(new Day03());
    }
}
