package aoc2018;

import java.io.File;
import java.io.IOException;
import java.util.Optional;

import common2.AocBaseRunner;
import common2.AocPuzzleInfo;
import common2.AocResult;
import common2.IAocPuzzle;

public class Day14 implements IAocPuzzle<Integer, String, Integer> {

    @Override
    public AocPuzzleInfo getInfo() {
        return new AocPuzzleInfo(2018, 14, "Chocolate Charts", false);
    }

    @Override
    public AocResult<String, Integer> getExpected() {
        return AocResult.of("5115114101", 20310465);
    }

    @Override
    public Integer parse(Optional<File> file) throws IOException {
        return 633601;
    }

    @Override
    public String part1(Integer input) {
        StringBuilder sb = new StringBuilder();
        sb.append("3");
        sb.append("7");
        int e1 = 0;
        int e2 = 1;

        while (true) {
            int es1 = sb.charAt(e1) - '0';
            int es2 = sb.charAt(e2) - '0';

            for (char c : String.valueOf(es1 + es2).toCharArray()) {
                sb.append(c);
            }

            e1 = (e1 + 1 + es1) % sb.length();
            e2 = (e2 + 1 + es2) % sb.length();

            if (sb.length() > (input + 10)) {
                return sb.substring(input, input + 10);
            }
        }
    }

    @Override
    public Integer part2(Integer input) {
        String inputStr = String.valueOf(input);
        StringBuilder sb = new StringBuilder();
        sb.append("3");
        sb.append("7");
        int e1 = 0;
        int e2 = 1;
        int next = 0;

        while (true) {
            int es1 = sb.charAt(e1) - '0';
            int es2 = sb.charAt(e2) - '0';

            for (char c : String.valueOf(es1 + es2).toCharArray()) {
                sb.append(c);

                if (c == inputStr.charAt(next)) {
                    next++;
                    if (next == inputStr.length()) {
                        return sb.length() - inputStr.length();
                    }
                } else {
                    next = 0;
                }
            }

            e1 = (e1 + 1 + es1) % sb.length();
            e2 = (e2 + 1 + es2) % sb.length();
        }
    }

    public static void main(String[] args) {
        AocBaseRunner.run(new Day14());
    }

}
