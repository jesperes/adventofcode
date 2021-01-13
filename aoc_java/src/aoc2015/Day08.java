package aoc2015;

import java.io.File;
import java.io.IOException;
import java.util.List;
import java.util.Optional;

import common2.AocPuzzleInfo;
import common2.AocResult;
import common2.IAocIntPuzzle;
import common2.InputUtils;

public class Day08 implements IAocIntPuzzle<List<String>> {

    int memoryChars(String line) {
        int chars = 0;
        for (int i = 1; i < line.length() - 1;) {
            String substr = line.substring(i);
            if (substr.startsWith("\\x")) {
                chars++;
                i += 4;
            } else if (substr.startsWith("\\")) {
                i += 2;
                chars++;
            } else {
                i++;
                chars++;
            }
        }
        return chars;
    }

    int literalChars(String line) {
        int chars = 2;
        for (int i = 0; i < line.length(); i++) {
            switch (line.charAt(i)) {
            case '\\':
            case '"':
                chars += 2;
                break;
            default:
                chars++;
                break;
            }
        }
        return chars;
    }

    @Override
    public AocPuzzleInfo getInfo() {
        return new AocPuzzleInfo(2015, 8, "Matchsticks", true);
    }

    @Override
    public AocResult<Integer, Integer> getExpected() {
        return AocResult.of(1371, 2117);
    }

    @Override
    public List<String> parse(Optional<File> file) throws IOException {
        return InputUtils.asStringList(file.get());
    }

    @Override
    public Integer part1(List<String> input) {
        int memChars = 0;
        int totalChars = 0;
        for (String line : input) {
            totalChars += line.length();
            memChars += memoryChars(line);
        }
        return totalChars - memChars;
    }

    @Override
    public Integer part2(List<String> input) {
        int litChars = 0;
        int totalChars = 0;
        for (String line : input) {
            totalChars += line.length();
            litChars += literalChars(line);
        }
        return litChars - totalChars;
    }
}
