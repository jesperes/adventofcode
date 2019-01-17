import java.io.BufferedReader;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.util.concurrent.TimeUnit;

public class Puzzle5 {
    public static void main(String[] args)
            throws FileNotFoundException, IOException {
        try (BufferedReader reader = new BufferedReader(
                new FileReader("input.txt"))) {
            String orig = reader.readLine();
            char[] buf = orig.toCharArray();

            long t0 = System.nanoTime();
            int len = react(buf);
            int part1 = len;

            int minlen = Integer.MAX_VALUE;
            StringBuilder s = new StringBuilder();
            for (char c = 'a'; c != 'z'; c++) {
                s.setLength(0);
                buf = orig.toCharArray();

                for (int i = 0; i < buf.length; i++) {
                    if (buf[i] != c && buf[i] != c - 32) {
                        s.append(buf[i]);
                    }
                }

                buf = s.toString().toCharArray();
                len = react(buf);
                minlen = Math.min(len, minlen);
            }
            long elapsed = System.nanoTime() - t0;
            int part2 = minlen;

            System.out.format("Part 1 = %d, Part 2 = %d, msecs = %d%n", part1,
                    part2, TimeUnit.NANOSECONDS.toMillis(elapsed));
        }
    }

    static private int react(char[] buf) {
        int l = buf.length;

        while (true) {
            int l0 = react_once(buf, l);

            if (l0 == l)
                return l0;
            else
                l = l0;
        }
    }

    static private int react_once(char[] buf, int len) {

        int src = 0;
        int tgt = 0;

        while (true) {
            if (src == len)
                break;

            if (src + 1 == len) {
                buf[tgt++] = buf[src++];
                break;
            }

            char x = buf[src];
            char y = buf[src + 1];

            if (Math.abs(x - y) == 32) {
                src += 2;
            } else {
                buf[tgt++] = buf[src++];
            }
        }

        return tgt;
    }
}
