package aoc2018;

import static common2.MapUtils.mapValueComparator;

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
        int maxx = input.stream().mapToInt(c -> c.x).max().getAsInt();
        int maxy = input.stream().mapToInt(c -> c.y).max().getAsInt();
        Set<Integer> infiniteAreas = new HashSet<>();
        Map<Integer, Integer> areas = new HashMap<>();

        for (int x = 0; x < maxx; x++) {
            for (int y = 0; y < maxy; y++) {
                int x0 = x;
                int y0 = y;
                var byDistance = input.stream().map(c -> {
                    int d = Math.abs(c.x - x0) + Math.abs(c.y - y0);
                    return new CoordDist(c.x, c.y, d, c.id);
                }).sorted().limit(2).collect(Collectors.toList());

                CoordDist closest = byDistance.get(0);
                CoordDist nextClosest = byDistance.get(1);

                if (closest.dist != nextClosest.dist) {
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
                .sorted(Collections.reverseOrder(mapValueComparator()))
                .findFirst().get().getValue();
    }

    @Override
    public Integer part2(List<Coord> input) {
        int maxx = input.stream().mapToInt(c -> c.x).max().getAsInt();
        int maxy = input.stream().mapToInt(c -> c.y).max().getAsInt();
        int size = 0;
        for (int x = 0; x < maxx; x++) {
            for (int y = 0; y < maxy; y++) {
                int d = 0;
                for (Coord c : input) {
                    d += (Math.abs(c.x - x) + Math.abs(c.y - y));
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
