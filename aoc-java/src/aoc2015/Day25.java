package aoc2015;

import static org.junit.Assert.assertEquals;

import org.junit.Test;

public class Day25 {

    @Test
    public void testDay25() throws Exception {
        int row = 1;
        int col = 1;
        long value = 20151125L;

        while (true) {
            value = (value * 252533L) % 33554393;

            if (row == 1) {
                row = col + 1;
                col = 1;
            } else {
                row--;
                col++;
            }

            if (row == 3010 && col == 3019)
                break;
        }

        assertEquals(8997277L, value);
    }
}
