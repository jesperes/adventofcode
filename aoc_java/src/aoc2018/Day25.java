package aoc2018;

import static java.lang.Math.abs;
import static org.junit.Assert.assertEquals;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

import org.junit.Test;

import common.AocPuzzle;

public class Day25 extends AocPuzzle {

    public Day25() {
        super(2018, 25);
    }

    static private final int MAX_CONSTELLATION_DISTANCE = 3;

    static private class Point {
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

    private List<Point> parsePointFile()
            throws FileNotFoundException, IOException {
        return getInputAsStream().map(line -> new Point(line))
                .collect(Collectors.toList());
    }

    @Test
    public void testPart() throws Exception {
        List<Point> points = parsePointFile();
        assertEquals(318, collectConstellations(points));
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
}
