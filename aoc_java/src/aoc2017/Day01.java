package aoc2017;

import java.io.File;
import java.io.IOException;
import java.util.Optional;

import common2.AocBaseRunner;
import common2.AocPuzzleInfo;
import common2.AocResult;
import common2.IAocIntPuzzle;
import common2.InputUtils;

public class Day01 implements IAocIntPuzzle<String> {

    @Override
    public AocPuzzleInfo getInfo() {
        return new AocPuzzleInfo(2017, 1, "Inverse Captcha", true);
    }

    @Override
    public AocResult<Integer, Integer> getExpected() {
        return AocResult.of(1102, 1076);
    }

    @Override
    public String parse(Optional<File> file) throws IOException {
        return InputUtils.asString(file.get()).trim();
    };

    @Override
    public Integer part1(String input) {
        int sum = 0;
        for (int i = 0; i < input.length(); i++) {
            char a = input.charAt(i);
            char b = input.charAt((i + 1) % input.length());
            if (a == b) {
                sum += (a - '0');
            }
        }
        return sum;
    }

    @Override
    public Integer part2(String input) {
        int sum = 0;
        int len = input.length();
        int half = len / 2;
        for (int i = 0; i < len; i++) {
            char a = input.charAt(i);
            char b = input.charAt((i + half) % len);
            if (a == b) {
                sum += (a - '0');
            }
        }
        return sum;
    }

    public static void main(String[] args) {
        AocBaseRunner.run(new Day01());
    }
}
