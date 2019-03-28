package aoc2016;

import static java.lang.Math.abs;
import static org.junit.Assert.assertEquals;

import java.io.BufferedReader;
import java.io.FileReader;
import java.util.HashSet;
import java.util.Set;

import org.junit.Test;

public class Day01 {
    String key(int x, int y) {
        return String.format("{%d,%d}", x, y);
    }

    @Test
    public void testDay01() throws Exception {
        Set<String> visited = new HashSet<>();
        int visitedTwice = -1;

        try (BufferedReader reader = new BufferedReader(
                new FileReader("inputs/2016/day01.txt"))) {

            int x = 0, y = 0;
            int dir = 0;
            visited.add(key(x, y));

            String input = reader.readLine();
            for (String s : input.split("[ ,]+")) {
                dir = ((s.charAt(0) == 'R' ? dir + 1 : dir - 1) + 4) % 4;
                int steps = Integer.valueOf(s.substring(1));
                int dx = 0, dy = 0;

                switch (dir) {
                case 0: // N
                    dy = -1;
                    break;
                case 1: // E
                    dx = 1;
                    break;
                case 2: // S
                    dy = 1;
                    break;
                case 3: // W
                    dx = -1;
                    break;
                }

                for (int i = 0; i < steps; i++) {
                    x += dx;
                    y += dy;

                    if (visitedTwice == -1) {
                        if (visited.contains(key(x, y))) {
                            visitedTwice = abs(x) + abs(y);
                        } else {
                            visited.add(key(x, y));
                        }
                    }
                }
            }

            assertEquals(161, visitedTwice);
            assertEquals(278, abs(x) + abs(y));
        }
    }
}
