package aoc2015;

import java.io.File;
import java.util.Optional;

import common2.AocPuzzleInfo;
import common2.AocResult;
import common2.IAocIntPuzzle;
import common2.InputUtils;

public class Day01 implements IAocIntPuzzle<String> {

    @Override
    public AocPuzzleInfo getInfo() {
        return new AocPuzzleInfo(2015, 1, "Not Quite Lisp", true);
    }

    @Override
    public AocResult<Integer, Integer> getExpected() {
        return AocResult.of(232, 1783);
    }

    @Override
    public String parse(Optional<File> file) {
        return InputUtils.asString(file.get());
    }

    @Override
    public Integer part1(String input) {
        int floor = 0;
        for (char ch : input.toCharArray()) {
            if (ch == '(')
                floor++;
            else if (ch == ')')
                floor--;
        }
        return floor;
    }

    @Override
    public Integer part2(String input) {
        int floor = 0;
        int pos = 1;
        for (char ch : input.toCharArray()) {
            if (ch == '(')
                floor++;
            else if (ch == ')')
                floor--;

            if (floor == -1) {
                return pos;
            } else {
                pos++;
            }
        }
        throw new RuntimeException();
    }
}
