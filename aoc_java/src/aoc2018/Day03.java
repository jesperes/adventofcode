package aoc2018;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.io.File;
import java.io.IOException;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

import aoc2018.Day03.Claim;
import common2.AocBaseRunner;
import common2.AocPuzzleInfo;
import common2.AocResult;
import common2.IAocIntPuzzle;
import common2.InputUtils;

public class Day03 implements IAocIntPuzzle<List<Claim>> {
    Pattern p = Pattern.compile(
            "#(?<id>\\d+) @ (?<x>\\d+),(?<y>\\d+): (?<w>\\d+)x(?<h>\\d+)");

    record Claim(int id, int x, int y, int w, int h) {
    }

    @Override
    public AocPuzzleInfo getInfo() {
        return new AocPuzzleInfo(2018, 3, "No Matter How You Slice It", true);
    }

    @Override
    public AocResult<Integer, Integer> getExpected() {
        return AocResult.of(105231, 164);
    }

    static int gtoi(Matcher m, String name) {
        return Integer.parseInt(m.group(name));
    }

    @Override
    public List<Claim> parse(Optional<File> file) throws IOException {
        return InputUtils.asStringList(file.get()).stream().map(line -> {
            var m = p.matcher(line);
            assertTrue(m.matches());
            int id = gtoi(m, "id");
            int x = gtoi(m, "x");
            int y = gtoi(m, "y");
            int w = gtoi(m, "w");
            int h = gtoi(m, "h");
            return new Claim(id, x, y, w, h);
        }).collect(Collectors.toList());
    }

    @Override
    public Integer part1(List<Claim> claims) {
        int size = 1000;
        int[][] grid = new int[size][size];

        for (Claim c : claims) {
            for (int x = c.x; x < c.x + c.w; x++) {
                for (int y = c.y; y < c.y + c.h; y++) {
                    grid[x][y]++;
                }
            }
        }

        int count = 0;
        for (int x = 0; x < size; x++) {
            for (int y = 0; y < size; y++) {
                if (grid[x][y] >= 2)
                    count++;
            }
        }
        return count;
    }

    @Override
    public Integer part2(List<Claim> claims) {
        // Store a map of square inches (their xy-keys) to a set of
        // the claims made on each square inch.
        Map<Integer, Set<Integer>> map = new HashMap<>();
        Set<Integer> nonOverlappingClaims = new HashSet<>();

        for (Claim c : claims) {
            nonOverlappingClaims.add(c.id);
            for (int x = c.x; x < c.x + c.w; x++) {
                for (int y = c.y; y < c.y + c.h; y++) {
                    // The key is here uninteresting; we just need a unique
                    // value for each (x, y) coordinate. Computing an integer
                    // is faster than creating a dedicated "coord" object.
                    map.compute(1000 * x + y, (k, v) -> {
                        if (v == null)
                            v = new HashSet<>();

                        v.add(c.id);
                        return v;
                    });
                }
            }
        }

        /*
         * For each square inch: if there are more than one claim on it, remove
         * the claim from the list of non-overlapping claims.
         */
        for (Set<Integer> set : map.values()) {
            if (set.size() > 1) {
                for (int id : set) {
                    nonOverlappingClaims.remove(id);
                }
            }
        }

        // When we are done, there should be only one claim left.
        assertEquals(1, nonOverlappingClaims.size());
        return nonOverlappingClaims.iterator().next();
    }

    public static void main(String[] args) {
        AocBaseRunner.run(new Day03());
    }
}
