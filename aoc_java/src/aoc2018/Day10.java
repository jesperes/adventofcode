package aoc2018;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.io.File;
import java.io.IOException;
import java.util.HashSet;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

import common2.AocBaseRunner;
import common2.AocPuzzleInfo;
import common2.AocResult;
import common2.IAocPuzzle;
import common2.InputUtils;

public class Day10 implements IAocPuzzle<List<String>, String, Integer> {

    Pattern p = Pattern.compile("position=<(.*),(.*)> velocity=<(.*),(.*)>");

    class Particle {
        int x;
        int y;
        final int dx;
        final int dy;

        public Particle(int x, int y, int dx, int dy) {
            this.x = x;
            this.y = y;
            this.dx = dx;
            this.dy = dy;
        }

        void move() {
            x += dx;
            y += dy;
        }
    }

    record Point(int x, int y) {
    }

    @Override
    public AocPuzzleInfo getInfo() {
        return new AocPuzzleInfo(2018, 10, "The Stars Align", true);
    }

    @Override
    public AocResult<String, Integer> getExpected() {
        return AocResult.of("JJXZHKFP", 10036);
    }

    @Override
    public List<String> parse(Optional<File> file) throws IOException {
        return InputUtils.asStringList(file.get());
    }

    @Override
    public String part1(List<String> input) {
        List<Particle> particles = getParticles(input);

        while (true) {
            if (isMessageShowing(particles)) {
                assertEquals(expectedMessage(), toString(particles));
                return "JJXZHKFP";
            }

            for (Particle particle : particles) {
                particle.move();
            }
        }
    }

    @Override
    public Integer part2(List<String> input) {
        List<Particle> particles = getParticles(input);
        int count;
        for (count = 0; !isMessageShowing(particles); count++) {
            for (Particle particle : particles) {
                particle.move();
            }
        }
        return count;
    }

    /*
     * Helpers
     */
    List<Particle> getParticles(List<String> lines) {
        return lines.stream().map(line -> {
            var m = p.matcher(line);
            assertTrue(m.matches());
            return new Particle(Integer.valueOf(m.group(1).trim()),
                    Integer.valueOf(m.group(2).trim()),
                    Integer.valueOf(m.group(3).trim()),
                    Integer.valueOf(m.group(4).trim()));
        }).collect(Collectors.toList());
    }

    boolean isMessageShowing(List<Particle> particles) {
        return circumference(particles) < 160;
    }

    int circumference(List<Particle> particles) {
        int maxx = Integer.MIN_VALUE;
        int maxy = Integer.MIN_VALUE;
        int minx = Integer.MAX_VALUE;
        int miny = Integer.MAX_VALUE;
        for (Particle p : particles) {
            maxx = Math.max(maxx, p.x);
            maxy = Math.max(maxy, p.y);
            minx = Math.min(minx, p.x);
            miny = Math.min(miny, p.y);
        }

        return 2 * ((maxx - minx) + (maxy - miny));
    }

    String expectedMessage() {
        // @formatter:off
        return //
        """
        ...###.....###..#....#..######..#....#..#....#..######..#####.
        ....#.......#...#....#.......#..#....#..#...#...#.......#....#
        ....#.......#....#..#........#..#....#..#..#....#.......#....#
        ....#.......#....#..#.......#...#....#..#.#.....#.......#....#
        ....#.......#.....##.......#....######..##......#####...#####.
        ....#.......#.....##......#.....#....#..##......#.......#.....
        ....#.......#....#..#....#......#....#..#.#.....#.......#.....
        #...#...#...#....#..#...#.......#....#..#..#....#.......#.....
        #...#...#...#...#....#..#.......#....#..#...#...#.......#.....
        .###.....###....#....#..######..#....#..#....#..#.......#.....
        """;
        // @formatter:on
    }

    private String toString(List<Particle> particles) {
        StringBuilder sb = new StringBuilder();
        Set<Point> points = new HashSet<>();
        int maxx = Integer.MIN_VALUE;
        int maxy = Integer.MIN_VALUE;
        int minx = Integer.MAX_VALUE;
        int miny = Integer.MAX_VALUE;
        for (Particle p : particles) {
            maxx = Math.max(maxx, p.x);
            maxy = Math.max(maxy, p.y);
            minx = Math.min(minx, p.x);
            miny = Math.min(miny, p.y);
            points.add(new Point(p.x, p.y));
        }

        for (int y = miny; y <= maxy; y++) {
            for (int x = minx; x <= maxx; x++) {
                if (points.contains(new Point(x, y))) {
                    sb.append("#");
                } else {
                    sb.append(".");
                }
            }
            sb.append("\n");
        }
        return sb.toString();
    }

    public static void main(String[] args) {
        AocBaseRunner.run(new Day10());
    }
}
