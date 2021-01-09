package aoc2020;

import java.io.File;
import java.util.List;
import java.util.Optional;

import aoc2020.Day02.PasswordData;
import common2.AocPuzzleInfo;
import common2.AocResult;
import common2.IAocPuzzle;
import common2.InputUtils;

public class Day02 implements IAocPuzzle<List<PasswordData>, Long, Long> {

    static class PasswordData {
        final int from;
        final int to;
        final char c;
        final String password;

        public PasswordData(String s) {
            /*
             * We could use String#split or e.g. Guava's Splitter, but they are
             * both rather sluggish, especially considering our input is very
             * strict.
             */
            int i = 0;
            while (s.charAt(i++) != '-')
                ;
            from = Integer.parseInt(s.substring(0, i - 1), 10);
            int n = i;
            while (s.charAt(i++) != ' ')
                ;
            to = Integer.parseInt(s.substring(n, i - 1), 10);
            while (s.charAt(i++) != ':')
                ;
            c = s.charAt(i - 2);
            password = s.substring(i + 1);
        }
    }

    @Override
    public List<PasswordData> parse(Optional<File> file) {
        return InputUtils.asStringList(file.get(), s -> new PasswordData(s));
    }

    @Override
    public Long part1(List<PasswordData> input) {
        return input.stream().filter(data -> isValidPart1(data)).count();
    }

    @Override
    public Long part2(List<PasswordData> input) {
        return input.stream().filter(data -> isValidPart2(data)).count();
    }

    @Override
    public AocResult<Long, Long> getExpected() {
        return AocResult.of(660L, 530L);
    }

    @Override
    public AocPuzzleInfo getInfo() {
        return new AocPuzzleInfo(2020, 2, "Password Philosophy", true);
    }

    /*
     * Helpers
     */

    private boolean isValidPart1(PasswordData data) {
        int n = 0;
        int len = data.password.length();
        for (int i = 0; i < len; i++) {
            if (data.c == data.password.charAt(i))
                n++;
        }
        return n >= data.from && n <= data.to;
    }

    private boolean isValidPart2(PasswordData data) {
        boolean c1 = data.password.charAt(data.from - 1) == data.c;
        boolean c2 = data.password.charAt(data.to - 1) == data.c;
        return (c1 == true && c2 == false) || (c1 == false && c2 == true);
    }
}
