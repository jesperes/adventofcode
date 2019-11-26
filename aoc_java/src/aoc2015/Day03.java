package aoc2015;

import static org.junit.Assert.assertEquals;

import java.io.FileReader;
import java.io.IOException;
import java.io.Reader;
import java.util.HashSet;
import java.util.Set;

import org.junit.Test;

import common.IntPair;

public class Day03 {

    class Santa {
        private int x, y;
        private Set<IntPair> houses;

        public Santa(int x, int y, Set<IntPair> houses) {
            this.x = x;
            this.y = y;
            this.houses = houses;
        }

        void move(int c) {
            switch (c) {
            case '<':
                x--;
                break;
            case '>':
                x++;
                break;
            case 'v':
                y++;
                break;
            case '^':
                y--;
                break;
            }
        }

        void deliver() {
            houses.add(new IntPair(x, y));
        }
    }

    @Test
    public void testDay03() throws IOException {
        Set<IntPair> p1 = new HashSet<>();
        Set<IntPair> p2 = new HashSet<>();

        Santa santa1 = new Santa(0, 0, p1);
        Santa santa2 = new Santa(0, 0, p2);
        Santa roboSanta = new Santa(0, 0, p2);

        try (Reader reader = new FileReader("inputs/2015/day03.txt")) {
            int c;
            int i = 0;
            while ((c = reader.read()) != -1) {
                santa1.deliver();
                santa2.deliver();
                roboSanta.deliver();

                santa1.move(c);
                if (i % 2 == 0)
                    santa2.move(c);
                else
                    roboSanta.move(c);

                i++;
            }
        }

        assertEquals(2572, p1.size());
        assertEquals(2631, p2.size());
    }
}
