package aoc2015;

import static org.junit.Assert.assertEquals;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;

import org.junit.Test;

public class Day02 {

    int min(int x1, int x2, int x3) {
        if (x1 < x2 && x1 < x3)
            return x1;
        else if (x2 < x3)
            return x2;
        else
            return x3;
    }

    @Test
    public void testDay02() throws IOException {
        int wrapping = 0;
        int ribbon = 0;

        try (BufferedReader reader = new BufferedReader(
                new FileReader("inputs/2015/day02.txt"))) {
            String line;
            while ((line = reader.readLine()) != null) {
                String[] elems = line.split("x");
                int x = Integer.valueOf(elems[0]);
                int y = Integer.valueOf(elems[1]);
                int z = Integer.valueOf(elems[2]);

                wrapping += (2 * x * y) + (2 * x * z) + (2 * z * y)
                        + min(x * y, y * z, z * x);
                ribbon += 2 * min(x + y, y + z, z + x) + (x * y * z);
            }
        }

        assertEquals(1586300, wrapping);
        assertEquals(3737498, ribbon);
    }
}
