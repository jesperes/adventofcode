package aoc2015;

import java.io.FileReader;
import java.io.IOException;
import java.io.Reader;
import java.util.HashSet;
import java.util.Set;

import org.junit.Test;

public class Day03 {

    class Coord {
        public int x, y;

        public Coord(int x, int y) {
            this.x = x;
            this.y = y;
        }

        void move(char c) {
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
    }

    @Test
    public void testDay03() throws IOException {
        Set<Integer> p1 = new HashSet<>();
        Set<Integer> p2 = new HashSet<>();

        Coord santa1 = new Coord(0, 0);
        Coord santa2 = new Coord(0, 0);
        Coord roboSanta = new Coord(0, 0);

        try (Reader reader = new FileReader("input/2015/day03.txt")) {
            int c;
            while ((c = reader.read()) != -1) {
            }
        }
    }
}
