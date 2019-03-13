package aoc2015;

import static org.junit.Assert.assertEquals;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;

import org.junit.Test;

import common.Block;

public class Day02 {

    int wrapping = 0;
    int ribbon = 0;

    class Present extends Block {

        public Present(int width, int depth, int height) {
            super(width, depth, height);
        }

        int sizeOfSmallestSide() {
            if (width > depth && width > height) {
                return depth * height;
            } else if (depth > width && depth > height) {
                return height * width;
            } else {
                return depth * width;
            }
        }

        int perimeterOfSmallestSide() {
            if (width > depth && width > height) {
                return 2 * depth + 2 * height;
            } else if (depth > width && depth > height) {
                return 2 * height + 2 * width;
            } else {
                return 2 * depth + 2 * width;
            }
        }

        int wrapping() {
            return 2 * width * height + 2 * height * depth + 2 * depth * width
                    + sizeOfSmallestSide();
        }

        int ribbon() {
            return perimeterOfSmallestSide() + height * depth * width;
        }
    }

    void start() throws IOException {
        try (BufferedReader reader = new BufferedReader(
                new FileReader("inputs/2015/day02.txt"))) {

            reader.lines().map(this::parseLine).forEach(present -> {
                wrapping += present.wrapping();
                ribbon += present.ribbon();
            });
        }
    }

    Present parseLine(String line) {
        String[] elems = line.split("x");
        return new Present(Integer.valueOf(elems[0]), Integer.valueOf(elems[1]),
                Integer.valueOf(elems[2]));
    }

    @Test
    public void testDay02() throws Exception {
        start();
        assertEquals(1586300, wrapping);
        assertEquals(3737498, ribbon);
    }

    @Test
    public void testPresent1() throws Exception {
        Present p = new Present(2, 3, 4);
        assertEquals(58, p.wrapping());
        assertEquals(34, p.ribbon());
    }

    @Test
    public void testPresent2() throws Exception {
        Present p = new Present(1, 1, 10);
        assertEquals(43, p.wrapping());
        assertEquals(14, p.ribbon());
    }
}
