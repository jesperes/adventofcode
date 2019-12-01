package aoc2018;

import static org.junit.Assert.assertEquals;

import org.junit.Test;

import common.AocPuzzle;

public class Day14 extends AocPuzzle {

    public Day14() {
        super(2018, 14);
    }

    @Test
    public void testPart1() throws Exception {
        assertEquals("5115114101", part1(633601));
    }

    @Test
    public void testPart2() throws Exception {
        assertEquals(20310465, part2("633601"));
    }

    private static String part1(int input) {
        StringBuilder scoreboard = new StringBuilder();
        scoreboard.append("3");
        scoreboard.append("7");
        int elf1 = 0;
        int elf2 = 1;

        while (true) {
            int elfscore1 = scoreboard.charAt(elf1) - '0';
            int elfscore2 = scoreboard.charAt(elf2) - '0';

            for (char c : String.valueOf(elfscore1 + elfscore2).toCharArray()) {
                scoreboard.append(c);
            }

            elf1 = (elf1 + 1 + elfscore1) % scoreboard.length();
            elf2 = (elf2 + 1 + elfscore2) % scoreboard.length();

            if (scoreboard.length() > (input + 10)) {
                return scoreboard.substring(input, input + 10);
            }
        }
    }

    private static int part2(String input) {
        StringBuilder scoreboard = new StringBuilder();
        scoreboard.append("3");
        scoreboard.append("7");
        int elf1 = 0;
        int elf2 = 1;
        int next = 0;

        while (true) {
            int elfscore1 = scoreboard.charAt(elf1) - '0';
            int elfscore2 = scoreboard.charAt(elf2) - '0';

            for (char c : String.valueOf(elfscore1 + elfscore2).toCharArray()) {
                scoreboard.append(c);

                if (c == input.charAt(next)) {
                    next++;
                    if (next == input.length()) {
                        return scoreboard.length() - input.length();
                    }
                } else {
                    next = 0;
                }
            }

            elf1 = (elf1 + 1 + elfscore1) % scoreboard.length();
            elf2 = (elf2 + 1 + elfscore2) % scoreboard.length();
        }
    }
}
