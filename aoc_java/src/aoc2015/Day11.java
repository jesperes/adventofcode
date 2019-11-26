package aoc2015;

import static org.junit.Assert.assertEquals;

import org.junit.Test;

public class Day11 {

    static final String INPUT = "cqjxjnds";

    private void increment(StringBuilder s) {
        for (int i = s.length() - 1; i >= 0; i--) {
            char c = s.charAt(i);
            if (c < 'z') {
                s.setCharAt(i, (char) (c + 1));
                break;
            } else {
                s.setCharAt(i, 'a');
            }
        }
    }

    private boolean hasStraight(String s) {
        for (int i = 2; i < s.length(); i++) {
            char a = s.charAt(i - 2);
            char b = s.charAt(i - 1);
            char c = s.charAt(i);
            if (a + 1 == b && b + 1 == c)
                return true;
        }
        return false;
    }

    private boolean hasConfusingChars(String s) {
        for (char c : s.toCharArray()) {
            switch (c) {
            case 'i':
            case 'o':
            case 'l':
                return true;
            }
        }
        return false;
    }

    private boolean hasPairs(String s) {
        int n = 0;
        for (int i = 1; i < s.length();) {
            if (s.charAt(i) == s.charAt(i - 1)) {
                n++;
                i += 2;

                if (n >= 2)
                    return true;
            } else {
                i++;
            }
        }
        return false;
    }

    private void nextPassword(StringBuilder b) {
        while (true) {
            increment(b);
            String s = b.toString();

            if (hasStraight(s) && !hasConfusingChars(s) && hasPairs(s)) {
                return;
            }
        }
    }

    @Test
    public void testDay11() throws Exception {
        StringBuilder password = new StringBuilder(INPUT);
        nextPassword(password);
        assertEquals("cqjxxyzz", password.toString());
        nextPassword(password);
        assertEquals("cqkaabcc", password.toString());
    }
}
