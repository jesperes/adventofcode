package aoc2015;

import static org.junit.Assert.assertEquals;

import java.io.BufferedReader;
import java.io.FileReader;

import org.junit.Test;

public class Day01 {
    @Test
    public void testDay01() throws Exception {
        int floor = 0;
        int basementPos = 0;

        try (BufferedReader reader = new BufferedReader(
                new FileReader("inputs/2015/day01.txt"))) {
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
        }

        assertEquals(232, floor);
        assertEquals(1783, basementPos);
    }
}
