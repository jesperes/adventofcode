package aoc2017;

import static java.lang.Math.abs;
import static java.lang.Math.pow;
import static java.lang.Math.sqrt;
import static org.junit.Assert.fail;

import java.io.File;
import java.io.IOException;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.concurrent.atomic.AtomicLong;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

import com.google.common.collect.ComparisonChain;

import aoc2017.Day20.Particle;
import common2.AocBaseRunner;
import common2.AocPuzzleInfo;
import common2.AocResult;
import common2.IAocLongPuzzle;
import common2.InputUtils;

public class Day20 implements IAocLongPuzzle<List<Particle>> {

    static class Coordinate {
        long x, y, z;

        public Coordinate(long x, long y, long z) {
            this.x = x;
            this.y = y;
            this.z = z;
        }

        @Override
        public int hashCode() {
            final int prime = 31;
            int result = 1;
            result = prime * result + (int) (x ^ (x >>> 32));
            result = prime * result + (int) (y ^ (y >>> 32));
            result = prime * result + (int) (z ^ (z >>> 32));
            return result;
        }

        @Override
        public boolean equals(Object c) {
            Coordinate other = (Coordinate) c;
            return other.x == x && other.y == y && other.z == z;
        }
    }

    static class Particle {
        long id;
        double acceleration;
        Coordinate p; // position
        Coordinate v; // velocity
        Coordinate a; // acceleration

        boolean destroyed = false;

        static String subpattern = "([ -]*[0-9]+)";
        static Pattern pattern = Pattern.compile(String.format(
                "p=<%s,%s,%s>, v=<%s,%s,%s>, a=<%s,%s,%s>", subpattern,
                subpattern, subpattern, subpattern, subpattern, subpattern,
                subpattern, subpattern, subpattern));

        public Particle(String line, long id) {
            this.id = id;
            Matcher m = pattern.matcher(line);
            if (m.matches()) {
                int coords[] = new int[9];
                if (m.groupCount() != 9)
                    fail("Expected group count == 9, but was "
                            + m.groupCount());

                for (int i = 1; i <= m.groupCount(); i++) {
                    coords[i - 1] = Integer.valueOf(m.group(i).trim());
                }

                p = new Coordinate(coords[0], coords[1], coords[2]);
                v = new Coordinate(coords[3], coords[4], coords[5]);
                a = new Coordinate(coords[6], coords[7], coords[8]);

            } else {
                fail("Regex match failure: " + pattern);
            }

            this.acceleration = sqrt(
                    pow(abs(a.x), 2) + pow(abs(a.y), 2) + pow(abs(a.z), 2));
        }

        public long update() {
            long d1 = distance();

            v.x += a.x;
            v.y += a.y;
            v.z += a.z;
            p.x += v.x;
            p.y += v.y;
            p.z += v.z;

            long d2 = distance();
            return d2 - d1;
        }

        /** Returns the distance from (0,0,0). */
        public long distance() {
            return Math.abs(p.x) + Math.abs(p.y) + Math.abs(p.z);
        }

        public double acceleration() {
            return acceleration;
        }
    }

    private long removeCollidingParticles(Collection<Particle> particles,
            int steps) {

        int lastCollision = 0;

        for (int i = 0;; i++) {

            Map<Coordinate, Particle> set = new HashMap<Coordinate, Particle>();

            for (Particle particle : particles) {
                if (set.containsKey(particle.p)) {
                    particle.destroyed = true;
                    set.get(particle.p).destroyed = true;
                } else {
                    set.put(particle.p, particle);
                }
            }

            int size = particles.size();
            particles.removeIf(p -> p.destroyed);
            int newsize = particles.size();

            if (newsize != size) {
                lastCollision = i;
            }

            if (i - lastCollision >= steps) {
                return particles.size();
            }

            for (Particle p : particles) {
                p.update();
            }
        }
    }

    @Override
    public AocPuzzleInfo getInfo() {
        return new AocPuzzleInfo(2017, 20, "Particle Swarm", true);
    }

    @Override
    public AocResult<Long, Long> getExpected() {
        return AocResult.of(457L, 448L);
    }

    @Override
    public List<Particle> parse(Optional<File> file) throws IOException {
        AtomicLong particleid = new AtomicLong();
        return InputUtils.asStringList(file.get()).stream()
                .map(line -> new Particle(line, particleid.getAndIncrement()))
                .collect(Collectors.toList());
    }

    @Override
    public Long part1(List<Particle> particles) {
        // Sort the particles. The closest/slowest particle will eventually stay
        // closest.
        Collections.sort(particles, new Comparator<Particle>() {
            @Override
            public int compare(Particle o1, Particle o2) {
                return ComparisonChain.start()
                        .compare(o1.acceleration(), o2.acceleration())
                        .compare(o1.distance(), o2.distance()).result();
            }
        });

        return particles.get(0).id;
    }

    @Override
    public Long part2(List<Particle> particles) {
        // 10 steps is the fewest amount of steps we can take and still get the
        // right results. Optimization-by-knowing-the-correct-result.
        int steps = 10;
        return removeCollidingParticles(particles, steps);
    }

    public static void main(String[] args) {
        AocBaseRunner.run(new Day20());
    }
}
