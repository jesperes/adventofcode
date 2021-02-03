package aoc2018;

import static java.lang.Character.toUpperCase;
import static java.lang.Math.abs;
import static java.lang.Math.min;

import java.io.File;
import java.io.IOException;
import java.util.Optional;

import common2.AocBaseRunner;
import common2.AocPuzzleInfo;
import common2.AocResult;
import common2.IAocIntPuzzle;
import common2.InputUtils;

public class Day05 implements IAocIntPuzzle<String> {

    @Override
    public AocPuzzleInfo getInfo() {
        return new AocPuzzleInfo(2018, 5, "Alchemical Reduction", true);
    }

    @Override
    public AocResult<Integer, Integer> getExpected() {
        return AocResult.of(10496, 5774);
    }

    @Override
    public String parse(Optional<File> file) throws IOException {
        return InputUtils.asString(file.get());
    }

    @Override
    public Integer part1(String input) {
        return react(input.toCharArray(), input.length());
    }

    @Override
    public Integer part2(String input) {
        int minLen = Integer.MAX_VALUE;
        int len = input.length();
        char[] buf = new char[len];

        /*
         * Check each polymer type, remove them one by one, and see which one
         * yields the shortest reacted string.
         */
        for (char c = 'a'; c <= 'z'; c++) {
            char C = toUpperCase(c);
            int j = 0;

            for (int i = 0; i < len; i++) {
                char x = input.charAt(i);
                if (x != c && x != C)
                    buf[j++] = x;
            }

            minLen = min(minLen, react(buf, j));
        }

        return minLen;
    }

    static private int react(char[] buf, int l) {
        int l0;
        while ((l0 = react_once(buf, l)) != l) {
            l = l0;
        }
        return l0;
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

            if (abs(x - y) == 32) {
                src += 2;
            } else {
                buf[tgt++] = buf[src++];
            }
        }

        return tgt;
    }

    public static void main(String[] args) {
        AocBaseRunner.run(new Day05());
    }
}
