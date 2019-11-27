package aoc2017;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;

import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.concurrent.atomic.AtomicLong;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

import org.junit.Test;

import common.AocPuzzle;

public class Day20 extends AocPuzzle {

    public Day20() {
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
        public boolean equals(Object obj) {
            if (this == obj)
                return true;
            if (obj == null)
                return false;
            if (getClass() != obj.getClass())
                return false;
            Coordinate other = (Coordinate) obj;
            if (x != other.x)
                return false;
            if (y != other.y)
                return false;
            if (z != other.z)
                return false;
            return true;
        }
    }

    static class Particle {
        long id;
        long acceleration;
        Coordinate p; // position
        Coordinate v; // velocity
        Coordinate a; // acceleration

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

            this.acceleration = Math.abs(a.x) + Math.abs(a.y) + Math.abs(a.z);
        }

        @Override
        public String toString() {
            return String.format("Particle[id = %d, dist = %d]", id,
                    distance());
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

        public long acceleration() {
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

        // Guess that 10000 is enough to figure out which particle is closest.
        for (int i = 0; i < 10000; i++) {
            particles.stream().forEach(p -> p.update());
        }

        Collections.sort(particles, new Comparator<Particle>() {
            @Override
            public int compare(Particle o1, Particle o2) {
                return Long.compare(o1.distance(), o2.distance());
            }
        });

        particles.stream().forEach(System.out::println);
    }
}
