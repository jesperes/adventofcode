package aoc2018;

import static java.lang.Math.abs;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

import aoc2018.Day25.Point;
import common2.AocBaseRunner;
import common2.AocPuzzleInfo;
import common2.AocResult;
import common2.IAocIntPuzzle;
import common2.InputUtils;

public class Day25 implements IAocIntPuzzle<List<Point>> {

    static private final int MAX_CONSTELLATION_DISTANCE = 3;

    class Point {
        final int x, y, z, w;
        Constellation constellation;

        Point(String line) {
            String[] s = line.split(",");
            x = Integer.valueOf(s[0]);
            y = Integer.valueOf(s[1]);
            z = Integer.valueOf(s[2]);
            w = Integer.valueOf(s[3]);
        }

        @Override
        public boolean equals(Object obj) {
            Point o = (Point) obj;
            return x == o.x && y == o.y && z == o.z && w == o.w;
        }

        @Override
        public int hashCode() {
            return x ^ y ^ z ^ w;
        }

        public int distanceTo(Point p) {
            return abs(x - p.x) + abs(y - p.y) + abs(z - p.z) + abs(w - p.w);
        }

        @Override
        public String toString() {
            return String.format("{%d,%d,%d,%d,c=%d}", x, y, z, w,
                    constellation.id);
        }
    }

    static private class Constellation {
        static int nextId = 1;
        int id = nextId++;
        Set<Point> points = new HashSet<>();
        boolean absorbed = false;

        public Constellation(Point p) {
            points.add(p);
            p.constellation = this;
        }

        private boolean closeEnoughTo(Point other) {
            return points.stream().anyMatch(
                    p -> p.distanceTo(other) <= MAX_CONSTELLATION_DISTANCE);
        }

        @Override
        public String toString() {
            return String.format("Constellation[id=%d, points=%s]", id,
                    points.toString());
        }

        boolean absorb(Constellation other) {
            if (equals(other) || other.absorbed)
                return false;

            if (other.points.stream().anyMatch(p -> closeEnoughTo(p))) {
                points.addAll(other.points);
                points.forEach(p -> p.constellation = this);
                other.points.clear();
                other.absorbed = true;
                return true;
            } else {
                return false;
            }
        }
    }

    private static int collectConstellations(List<Point> points) {
        List<Constellation> constellations = new ArrayList<>();

        // First, add all points to its own constellation
        for (Point p : points) {
            Constellation c = new Constellation(p);
            constellations.add(c);
        }

        // Second, iterate of the constellations until there are no
        // more merges to do.
        int merges;
        do {
            merges = 0;

            for (Constellation a : constellations) {
                for (Constellation b : constellations) {
                    if (a.absorb(b))
                        merges++;
                }
            }

            constellations.removeIf(c -> c.absorbed);

        } while (merges > 0);

        return constellations.size();
    }

    @Override
    public AocPuzzleInfo getInfo() {
        return new AocPuzzleInfo(2018, 25, "Four-Dimensional Adventure", true);
    }

    @Override
    public AocResult<Integer, Integer> getExpected() {
        return AocResult.of(318, null);
    }

    @Override
    public List<Point> parse(Optional<File> file) throws IOException {
        return InputUtils.asStringList(file.get()).stream().map(Point::new)
                .collect(Collectors.toList());
    }

    @Override
    public Integer part1(List<Point> input) {
        return collectConstellations(input);
    }

    @Override
    public Integer part2(List<Point> input) {
        return 0;
    }

    public static void main(String[] args) {
        AocBaseRunner.run(new Day25());
    }
}
