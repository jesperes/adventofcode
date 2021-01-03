package aoc2020.solutions;

import static java.lang.Math.max;
import static java.lang.Math.min;

import java.io.File;
import java.io.FileNotFoundException;
import java.util.HashSet;
import java.util.Optional;
import java.util.Set;

import aoc2020.AocBaseRunner;
import aoc2020.AocPuzzleInfo;
import aoc2020.AocResult;
import aoc2020.IAocIntPuzzle;
import aoc2020.solutions.Day17.ConwayCoord;

public class Day17 implements IAocIntPuzzle<Set<ConwayCoord>> {

    record ConwayCoord(int x, int y, int z, int w) {

    }

    @Override
    public AocPuzzleInfo getInfo() {
        return new AocPuzzleInfo(2020, 17, "Conway Cubes", false);
    }

    @Override
    public AocResult<Long, Long> getExpected() {
        return AocResult.of(242L, 2292L);
    }

    @Override
    public Set<ConwayCoord> parse(Optional<File> file) {
        String[] input = """
                ..#..#..
                .###..#.
                #..##.#.
                #.#.#.#.
                .#..###.
                .....#..
                #...####
                ##....#.
                """.split("\n");

        /*
         * Since the grid is infinite and possibly (very) sparse (z != 0 is
         * empty), we only store active cubes.
         */
        int y = 0;
        Set<ConwayCoord> coords = new HashSet<>();
        for (String line : input) {
            int x = 0;
            for (char c : line.toCharArray()) {
                if (c == '#')
                    coords.add(new ConwayCoord(x, y, 0, 0));
                x++;
            }
            y++;
        }
        return coords;
    }

    @Override
    public Long part1(Set<ConwayCoord> input) {
        return runSixCycles(input, 1);
    }

    @Override
    public Long part2(Set<ConwayCoord> input) {
        return runSixCycles(input, 2);
    }

    private long runSixCycles(Set<ConwayCoord> input, int part) {
        // Make copy of input since we intend to change it
        Set<ConwayCoord> coords = new HashSet<>();
        coords.addAll(input);

        // Run 6 steps.
        for (int i = 0; i < 6; i++) {
            conway(coords, part);
        }

        return (long) coords.size();
    }

    private void conway(Set<ConwayCoord> coords, int part) {
        int minx = 0, maxx = 0, miny = 0, maxy = 0, minz = 0, maxz = 0,
                minw = 0, maxw = 0;

        // Find a minimal cuboid containing all active cubes, so we know
        // which coordinates which could become active in the next generation
        for (ConwayCoord coord : coords) {
            minx = min(minx, coord.x - 1);
            maxx = max(maxx, coord.x + 1);
            miny = min(miny, coord.y - 1);
            maxy = max(maxy, coord.y + 1);
            minz = min(minz, coord.z - 1);
            maxz = max(maxz, coord.z + 1);
            if (part == 2) {
                minw = min(minw, coord.w - 1);
                maxw = max(maxw, coord.w + 1);
            }
        }

        Set<ConwayCoord> additions = new HashSet<>();
        Set<ConwayCoord> removals = new HashSet<>();

        // Compute additions and removals
        for (int x = minx; x <= maxx; x++) {
            for (int y = miny; y <= maxy; y++) {
                for (int z = minz; z <= maxz; z++) {
                    if (part == 1) {
                        checkAdjacents(new ConwayCoord(x, y, z, 0), coords,
                                additions, removals, part);
                    } else {
                        for (int w = minw; w <= maxw; w++) {
                            checkAdjacents(new ConwayCoord(x, y, z, w), coords,
                                    additions, removals, part);
                        }
                    }
                }
            }
        }

        // apply changes
        for (var c : removals) {
            coords.remove(c);
        }
        for (var c : additions) {
            coords.add(c);
        }
    }

    private void checkAdjacents(ConwayCoord cc, Set<ConwayCoord> coords,
            Set<ConwayCoord> additions, Set<ConwayCoord> removals, int part) {

        int n = 0;

        for (int a = cc.x - 1; a <= cc.x + 1; a++) {
            for (int b = cc.y - 1; b <= cc.y + 1; b++) {
                for (int c = cc.z - 1; c <= cc.z + 1; c++) {
                    if (part == 1) {
                        if (a == cc.x && b == cc.y && c == cc.z)
                            continue;

                        if (coords.contains(new ConwayCoord(a, b, c, 0)))
                            n++;
                    } else {
                        for (int d = cc.w - 1; d <= cc.w + 1; d++) {
                            if (a == cc.x && b == cc.y && c == cc.z
                                    && d == cc.w)
                                continue;

                            if (coords.contains(new ConwayCoord(a, b, c, d)))
                                n++;
                        }
                    }
                }
            }
        }

        if (coords.contains(cc)) {
            if (n != 2 && n != 3)
                removals.add(cc);
        } else if (n == 3) {
            additions.add(cc);
        }
    }

    public static void main(String[] args) throws FileNotFoundException {
        AocBaseRunner.run(new Day17());
    }
}
