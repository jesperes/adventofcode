package aoc2015;

import static org.junit.Assert.assertEquals;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;

import org.junit.Test;

public class Day06 {
    enum Mode {
        Toggle, TurnOn, TurnOff
    }

    @Test
    public void testDay06() throws IOException {
        try (BufferedReader reader = new BufferedReader(
                new FileReader("inputs/2015/day06.txt"))) {

            boolean grid1[][] = new boolean[1000][1000];
            int grid2[][] = new int[1000][1000];

            for (int x = 0; x <= 999; x++) {
                for (int y = 0; y <= 999; y++) {
                    grid1[x][y] = false;
                    grid2[x][y] = 0;
                }
            }

            String line;
            while ((line = reader.readLine()) != null) {
                String[] elems = line.split("[ ,]");
                Mode mode;
                int i;
                if (elems[0].equals("toggle")) {
                    mode = Mode.Toggle;
                    i = 1;
                } else if (elems[1].equals("on")) {
                    mode = Mode.TurnOn;
                    i = 2;
                } else {
                    mode = Mode.TurnOff;
                    i = 2;
                }

                int x1 = Integer.valueOf(elems[i]);
                int y1 = Integer.valueOf(elems[i + 1]);
                int x2 = Integer.valueOf(elems[i + 3]);
                int y2 = Integer.valueOf(elems[i + 4]);

                for (int x = x1; x <= x2; x++) {
                    for (int y = y1; y <= y2; y++) {
                        switch (mode) {
                        case Toggle:
                            grid1[x][y] = !grid1[x][y];
                            grid2[x][y] = grid2[x][y] + 2;
                            break;
                        case TurnOn:
                            grid1[x][y] = true;
                            grid2[x][y]++;
                            break;
                        case TurnOff:
                            grid1[x][y] = false;
                            grid2[x][y] = Math.max(0, grid2[x][y] - 1);
                            break;
                        }
                    }
                }
            }

            int numLit = 0;
            int totalBrightness = 0;

            for (int x = 0; x <= 999; x++) {
                for (int y = 0; y <= 999; y++) {
                    numLit += grid1[x][y] ? 1 : 0;
                    totalBrightness += grid2[x][y];
                }
            }

            assertEquals(543903, numLit);
            assertEquals(14687245, totalBrightness);
        }
    }
}
