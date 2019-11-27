package aoc2017;

import static java.lang.Math.abs;
import static java.lang.Math.pow;
import static java.lang.Math.sqrt;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.atomic.AtomicLong;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

import org.junit.Test;

import common.AocPuzzle;

public class Day20_Part2 extends AocPuzzle {

    public Day20_Part2() {
        super(2017, 20);
    }

    static class Coordinate {
        long x, y, z;

        public Coordinate(long x, long y, long z) {
            this.x = x;
            this.y = y;
            this.z = z;
        }

        @Override
        public String toString() {
            return "Coordinate [x=" + x + ", y=" + y + ", z=" + z + "]";
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

        @Override
        public String toString() {
            return String.format("Particle[id = %d, dist = %d, accel = %s/%g]",
                    id, distance(), a, acceleration());
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

    @Test
    public void testParseParticle() throws Exception {
        Particle particle = new Particle("p=< 3,0,0>, v=< 2,0,0>, a=<-1,0,0>",
                0L);
        assertEquals(new Coordinate(3, 0, 0), particle.p);
        assertEquals(new Coordinate(2, 0, 0), particle.v);
        assertEquals(new Coordinate(-1, 0, 0), particle.a);
    }

    @Test
    public void testLarge() throws Exception {
        AtomicLong particleid = new AtomicLong();

        List<Particle> particles = getInputAsStream()
                .map(line -> new Particle(line, particleid.getAndIncrement()))
                .collect(Collectors.toList());

        /*
         * Sort first by acceleration, then by distance. The closest/slowest
         * particle will eventually stay closest.
         */
        Collections.sort(particles, new Comparator<Particle>() {
            @Override
            public int compare(Particle o1, Particle o2) {
                int accel = Double.compare(o1.acceleration(),
                        o2.acceleration());
                if (accel != 0) {
                    return accel;
                } else {
                    return Long.compare(o1.distance(), o2.distance());
                }
            }
        });

        Particle particle = particles.get(0);
        assertEquals(457L, particle.id);
    }

    private int removeCollidingParticles(Collection<Particle> particles,
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
                System.out.format(
                        "In step %d, %d particles collided and were removed, leaving %d particles.%n",
                        i, size - newsize, newsize);
                lastCollision = i;
            }

            if (i - lastCollision >= steps) {
                System.out.format(
                        "No collisions in %d steps. Returning remaining particle: %d%n",
                        i - lastCollision, particles.size());
                return particles.size();
            }

            for (Particle p : particles) {
                p.update();
            }
        }
    }

    @Test
    public void testCollisionsShort() throws Exception {
        List<Particle> particles = new ArrayList<>();

        particles.add(new Particle("p=<-6,0,0>, v=< 3,0,0>, a=< 0,0,0>", 0L));
        particles.add(new Particle("p=<-4,0,0>, v=< 2,0,0>, a=< 0,0,0>", 0L));
        particles.add(new Particle("p=<-2,0,0>, v=< 1,0,0>, a=< 0,0,0>", 0L));
        particles.add(new Particle("p=< 3,0,0>, v=< 1,0,0>, a=< 0,0,0>", 0L));

        assertEquals(1, removeCollidingParticles(particles, 10));
    }

    @Test
    public void testCollisions() throws Exception {
        AtomicLong particleid = new AtomicLong();
        List<Particle> particles = getInputAsStream()
                .map(line -> new Particle(line, particleid.getAndIncrement()))
                .collect(Collectors.toList());

        removeCollidingParticles(particles, 1000);
    }
}
