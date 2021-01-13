package aoc2015;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

import common2.AocBaseRunner;
import common2.AocPuzzleInfo;
import common2.AocResult;
import common2.IAocIntPuzzle;

public class Day10 implements IAocIntPuzzle<String> {
    private int prefixLen(List<Character> buf) {
        // Hot code
        char c = buf.get(0);
        int len = 0;
        int size = buf.size();
        for (; len < size && buf.get(len) == c; len++) {
        }
        return len;
    }

    private void lookAndSay(List<Character> buf1, List<Character> buf2) {
        buf2.clear();
        int size = buf1.size();
        for (int i = 0; i < size;) {
            // Hot code
            int l = prefixLen(buf1.subList(i, size));
            char c = buf1.get(i);
            String sl = String.valueOf(l);
            int sl_len = sl.length();
            for (int j = 0; j < sl_len; j++) {
                buf2.add(sl.charAt(j));
            }
            buf2.add(c);
            i += l;
        }
    }

    @Override
    public AocPuzzleInfo getInfo() {
        return new AocPuzzleInfo(2015, 10, "Elves Look, Elves Say", false);
    }

    @Override
    public AocResult<Integer, Integer> getExpected() {
        return AocResult.of(492982, 6989950);
    }

    @Override
    public String parse(Optional<File> file) throws IOException {
        return "1321131112";
    }

    private int play(String input, int limit) {
        List<Character> buf1 = new ArrayList<>(1_000_000);
        List<Character> buf2 = new ArrayList<>(1_000_000);

        for (char c : input.toCharArray()) {
            buf1.add(c);
        }

        for (int i = 0; i < limit; i++) {
            lookAndSay(buf1, buf2);
            var tmp = buf1;
            buf1 = buf2;
            buf2 = tmp;
        }

        return buf1.size();
    }

    @Override
    public Integer part1(String input) {
        return play(input, 40);
    }

    @Override
    public Integer part2(String input) {
        return play(input, 50);
    }

    public static void main(String[] args) {
        AocBaseRunner.run(new Day10());
    }
}
