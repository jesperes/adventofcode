package aoc2015;

import static org.junit.Assert.assertEquals;

import java.io.FileReader;
import java.io.IOException;
import java.io.Reader;

import org.junit.Test;

public class Day01 {

    int floor = 0;
    int basementPos = 0;

    int start() throws IOException {
        try (Reader reader = new FileReader("inputs/2015/day01.txt")) {
            int c;
            int i = 0;
            while ((c = reader.read()) != -1) {
                switch (c) {
                case '(':
                    floor++;
                    break;
                case ')':
                    floor--;
                    break;
                }
                if (floor < 0 && basementPos == 0) {
                    basementPos = i + 1;
                }

                i++;
            }

            return floor;
        }
    }

    @Test
    public void testDay01() throws Exception {
        start();
        assertEquals(232, floor);
        assertEquals(1783, basementPos);
    }
}
