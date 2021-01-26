package aoc2017;

import static org.junit.Assert.fail;

import java.io.File;
import java.io.IOException;
import java.util.Optional;

import common2.AocBaseRunner;
import common2.AocPuzzleInfo;
import common2.AocResult;
import common2.IAocIntPuzzle;
import common2.InputUtils;

public class Day09 implements IAocIntPuzzle<String> {

    int interpret(String input, boolean part2) {

        boolean isGarbage = false;
        boolean cancelNextChar = false;
        int nestingLevel = 0;
        int score = 0;
        int numGarbageChars = 0;

        for (char c : input.toCharArray()) {
            if (isGarbage) {
                if (cancelNextChar) {
                    cancelNextChar = false;
                    continue;
                }

                if (c == '!') {
                    // ! cancels next character within garbage
                    cancelNextChar = true;
                    continue;
                } else if (c == '>') {
                    isGarbage = false;
                    continue;
                }

                // any other characters within garbage is ignored
                numGarbageChars++;

            } else {
                // Not garbage
                if (c == '<') {
                    // begin garbage
                    isGarbage = true;
                    continue;
                } else if (c == '{') {
                    nestingLevel++;
                } else if (c == '}') {
                    score += nestingLevel;
                    nestingLevel--;
                } else if (c == ',') {
                    // separates groups
                } else {
                    fail("unexpected character: " + c);
                }
            }
        }

        if (nestingLevel != 0)
            fail("Mismatched {, }, nesting level at end was " + nestingLevel);

        if (isGarbage)
            fail("String ended in garbage");

        if (part2)
            return numGarbageChars;
        else
            return score;
    }

    @Override
    public AocPuzzleInfo getInfo() {
        return new AocPuzzleInfo(2017, 9, "Stream Processing", true);
    }

    @Override
    public AocResult<Integer, Integer> getExpected() {
        return AocResult.of(7616, 3838);
    }

    @Override
    public String parse(Optional<File> file) throws IOException {
        return InputUtils.asString(file.get()).trim();
    }

    @Override
    public Integer part1(String input) {
        return interpret(input, false);
    }

    @Override
    public Integer part2(String input) {
        return interpret(input, true);
    }

    public static void main(String[] args) {
        AocBaseRunner.run(new Day09());
    }
}
