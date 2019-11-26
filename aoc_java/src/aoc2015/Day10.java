package aoc2015;

import static org.junit.Assert.assertEquals;

import java.util.ArrayList;
import java.util.List;

import org.junit.Test;

public class Day10 {
    static final String INPUT = "1321131112";

    private int prefixLen(List<Character> buf) {
        // Hot code
        char c = buf.get(0);
        int len = 0;
        int size = buf.size();
        for (; len < size && buf.get(len) == c; len++) {
        }
        return len;
    }

    private void lookAndSay(List<Character> buf1, List<Character> buf2) {
        buf2.clear();
        int size = buf1.size();
        for (int i = 0; i < size;) {
            // Hot code
            int l = prefixLen(buf1.subList(i, size));
            char c = buf1.get(i);
            String sl = String.valueOf(l);
            int sl_len = sl.length();
            for (int j = 0; j < sl_len; j++) {
                buf2.add(sl.charAt(j));
            }
            buf2.add(c);
            i += l;
        }
    }

    @Test
    public void testDay10() {
        List<Character> buf1 = new ArrayList<>();
        List<Character> buf2 = new ArrayList<>();

        for (char c : INPUT.toCharArray()) {
            buf1.add(c);
        }

        int part1Limit = 40;
        int part2Limit = 50;
        for (int i = 0; i < part2Limit; i++) {
            if (i == part1Limit) {
                assertEquals(492982, buf1.size());
            }

            lookAndSay(buf1, buf2);
            List<Character> tmp = buf1;
            buf1 = buf2;
            buf2 = tmp;
        }

        assertEquals(6989950, buf1.size());
    }
}
