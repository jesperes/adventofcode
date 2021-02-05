package aoc2018;

import static java.lang.Math.abs;

import java.io.File;
import java.io.IOException;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.stream.Collectors;

import aoc2018.Day06.Coord;
import common2.AocBaseRunner;
import common2.AocPuzzleInfo;
import common2.AocResult;
import common2.IAocIntPuzzle;
import common2.InputUtils;
import common2.MapUtils;

public class Day06 implements IAocIntPuzzle<List<Coord>> {

    record Coord(int x, int y, int id) {
    }

    record CoordDist(int x, int y, int dist, int id)
            implements Comparable<CoordDist> {
        @Override
        public int compareTo(CoordDist o) {
            return Integer.compare(dist, o.dist);
        }
    }

    @Override
    public AocPuzzleInfo getInfo() {
        return new AocPuzzleInfo(2018, 6, "Chronal Coordinates", true);
    }

    @Override
    public AocResult<Integer, Integer> getExpected() {
        return AocResult.of(3894, 39398);
    }

    @Override
    public List<Coord> parse(Optional<File> file) throws IOException {
        AtomicInteger count = new AtomicInteger();
        return InputUtils.asStringList(file.get()).stream().map(line -> {
            String[] elems = line.split("[, ]+");
            return new Coord(Integer.parseInt(elems[0]),
                    Integer.parseInt(elems[1]), count.incrementAndGet());
        }).collect(Collectors.toUnmodifiableList());
    }

    @Override
    public Integer part1(List<Coord> input) {
        int maxx = 356;
        int maxy = 353;
        Set<Integer> infiniteAreas = new HashSet<>();
        Map<Integer, Integer> areas = new HashMap<>();

        for (int x = 0; x < maxx; x++) {
            for (int y = 0; y < maxy; y++) {
                Coord closest = null; // closest coordinate
                int d0 = Integer.MAX_VALUE; // distance to closest coord
                int d1 = Integer.MAX_VALUE; // distance to next-closest coord

                // Check which coordinate is closest, and also track if
                // there is a tie
                for (Coord c : input) {
                    int d = abs(c.x - x) + abs(c.y - y);
                    if (d < d0) {
                        closest = c;
                        d1 = d0;
                        d0 = d;
                    } else if (d < d1) {
                        d1 = d;
                    }
                }

                if (d1 != d0) { // ignore ties
                    if (x == 0 || x == maxx - 1 || y == 0 || y == maxy - 1) {
                        infiniteAreas.add(closest.id);
                    } else {
                        areas.compute(closest.id,
                                (k, v) -> (v == null) ? 1 : v + 1);
                    }
                }
            }
        }

        return areas.entrySet().stream()
                .filter(e -> !infiniteAreas.contains(e.getKey()))
                .sorted(Collections.reverseOrder(MapUtils.mapValueComparator()))
                .findFirst().get().getValue();
    }

    @Override
    public Integer part2(List<Coord> input) {
        int maxx = 356;
        int maxy = 353;
        int size = 0;

        for (int x = 0; x < maxx; x++) {
            for (int y = 0; y < maxy; y++) {
                int d = 0;
                for (Coord c : input) {
                    d += (abs(c.x - x) + abs(c.y - y));
                }
                if (d < 10000) {
                    size++;
                }
            }
        }

        return size;
    }

    public static void main(String[] args) {
        AocBaseRunner.run(new Day06());
    }
}
