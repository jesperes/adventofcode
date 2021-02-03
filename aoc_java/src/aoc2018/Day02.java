package aoc2018;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.util.Optional;

import common2.AocBaseRunner;
import common2.AocPuzzleInfo;
import common2.AocResult;
import common2.IAocPuzzle;

public class Day02 implements IAocPuzzle<String[], Integer, String> {

    @Override
    public AocPuzzleInfo getInfo() {
        return new AocPuzzleInfo(2018, 2, "Inventory Management System", true);
    }

    @Override
    public AocResult<Integer, String> getExpected() {
        return AocResult.of(9139, "uqcidadzwtnhsljvxyobmkfyr");
    }

    @Override
    public String[] parse(Optional<File> file) throws IOException {
        return Files.lines(file.get().toPath()).toArray(n -> new String[n]);
    }

    @Override
    public Integer part1(String[] input) {
        int twos = 0, threes = 0;

        for (String s : input) {
            int[] array = new int[26];
            for (int i = 0; i < s.length(); i++) {
                array[s.charAt(i) - 'a']++;
            }

            int hasTwos = 0;
            int hasThrees = 0;

            for (int i = 0; i < array.length; i++) {
                int j = array[i];
                if (j == 2)
                    hasTwos = 1;
                if (j == 3)
                    hasThrees = 1;
            }

            twos += hasTwos;
            threes += hasThrees;
        }

        return twos * threes;
    }

    @Override
    public String part2(String[] lines) {
        for (String a : lines) {
            int len = a.length();
            for (String b : lines) {
                if (a == b)
                    continue;

                int count = 0;
                char c = 0;

                for (int i = 0; i < len; i++) {
                    if (a.charAt(i) != b.charAt(i)) {
                        count++;
                        c = a.charAt(i);
                    }
                }

                if (count == 1) {
                    return a.replace(String.valueOf(c), "");
                }
            }
        }
        throw new RuntimeException();
    }

    public static void main(String[] args) {
        AocBaseRunner.run(new Day02());
    }
}
