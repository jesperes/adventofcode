package aoc2020;

import static org.junit.Assert.assertEquals;

import java.io.IOException;
import java.util.stream.Stream;

import org.junit.Test;

import common.AocPuzzle;

/**
 * AoC puzzle template.
 */
public class Day02 extends AocPuzzle {

    class PasswordData {
        int from;
        int to;
        char c;
        String password;

        public PasswordData(String s) {
            String[] elems = s.split("[:\\- ]+");
            assertEquals(4, elems.length);
            from = Integer.valueOf(elems[0]);
            to = Integer.valueOf(elems[1]);
            c = elems[2].charAt(0);
            password = elems[3];
        }
    }

    private Stream<PasswordData> passwords;

    public Day02() throws IOException {
        super(2020, 2);
        passwords = getInputAsStream().map(s -> new PasswordData(s));
    }

    private long part1() {
        return passwords.filter(data -> isValidPart1(data)).count();
    }

    private boolean isValidPart1(PasswordData data) {
        int n = 0;
        for (char c : data.password.toCharArray()) {
            if (c == data.c)
                n++;
        }
        return n >= data.from && n <= data.to;
    }

    private long part2() {
        return passwords.filter(data -> isValidPart2(data)).count();
    }

    private boolean isValidPart2(PasswordData data) {
        char[] a = data.password.toCharArray();
        boolean c1 = a[data.from - 1] == data.c;
        boolean c2 = a[data.to - 1] == data.c;
        return (c1 == true && c2 == false) || (c1 == false && c2 == true);
    }

    /*
     * Tests
     */

    @Test
    public void testPart1() throws Exception {
        assertEquals(660, part1());
    }

    @Test
    public void testPart2() throws Exception {
        assertEquals(530, part2());
    }

}
