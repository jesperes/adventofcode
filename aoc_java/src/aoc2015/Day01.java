package aoc2015;

import java.io.File;
import java.util.Optional;

import common2.AocPuzzleInfo;
import common2.AocResult;
import common2.IAocIntPuzzle;
import common2.InputUtils;

public class Day01 implements IAocIntPuzzle<byte[]> {

    @Override
    public AocPuzzleInfo getInfo() {
        return new AocPuzzleInfo(2015, 1, "Not Quite Lisp", true);
    }

    @Override
    public AocResult<Integer, Integer> getExpected() {
        return AocResult.of(232, 1783);
    }

    @Override
    public byte[] parse(Optional<File> file) {
        return InputUtils.asByteArray(file.get());
    }

    @Override
    public Integer part1(byte[] input) {
        int floor = 0;
        for (byte ch : input) {
            if (ch == '(')
                floor++;
            else if (ch == ')')
                floor--;
        }
        return floor;
    }

    @Override
    public Integer part2(byte[] input) {
        int floor = 0;
        int pos = 1;
        for (byte ch : input) {
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
