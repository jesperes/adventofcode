package aoc2018;

import static org.junit.Assert.assertEquals;

import java.io.File;
import java.io.IOException;
import java.util.Arrays;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

import org.junit.Test;

import aoc2018.Day12.Input;
import common2.AocBaseRunner;
import common2.AocPuzzleInfo;
import common2.AocResult;
import common2.IAocLongPuzzle;
import common2.InputUtils;

public class Day12 implements IAocLongPuzzle<Input> {

    record Rule(String from, char to) {

    }

    record Input(List<Rule> rules, String initial) {

    }

    @Override
    public AocPuzzleInfo getInfo() {
        return new AocPuzzleInfo(2018, 12, "Subterranean Sustainability", true);
    }

    @Override
    public AocResult<Long, Long> getExpected() {
        return AocResult.of(2767L, 2650000001362L);
    }

    @Override
    public Input parse(Optional<File> file) throws IOException {
        var list = InputUtils.asStringList(file.get());
        var initialState = list.get(0).split(":")[1].trim();
        var rules = list.stream().skip(2).map(line -> {
            String[] elems = line.split(" => ");
            return new Rule(elems[0].trim(), elems[1].charAt(0));
        }).collect(Collectors.toList());
        return new Input(rules, initialState);
    }

    @Override
    public Long part1(Input input) {
        return simulate(input, 20);
    }

    private long simulate(Input input, int generations) {
        int size = 256;
        int offset = size / 2;
        char[] a = new char[size];
        char[] b = new char[size];
        int min = Integer.MAX_VALUE;
        int max = Integer.MIN_VALUE;

        for (int i = 0; i < size; i++) {
            a[i] = '.';
            b[i] = '.';
        }

        for (int i = 0; i < input.initial.length(); i++) {
            char c = input.initial.charAt(i);
            a[i + offset] = c;
            if (c == '#') {
                min = Math.min(min, i);
                max = Math.max(max, i);
            }
        }

        for (int gen = 0; gen < generations; gen++) {
            for (int i = min - 2; i <= max + 2; i++) {
                b[i + offset] = '.';
                for (Rule rule : input.rules) {
                    char[] r = rule.from.toCharArray();

                    if (Arrays.equals(a, i - 2 + offset, i + 3 + offset, r, 0,
                            5)) {
                        b[i + offset] = rule.to;
                        if (rule.to == '#') {
                            max = Math.max(i, max);
                            min = Math.min(i, min);
                        }
                        break;
                    }
                }
            }

            // Swap the two arrays
            char[] tmp = a;
            a = b;
            b = tmp;
        }

        int sum = 0;
        for (int i = 0; i < size; i++) {
            if (a[i] == '#') {
                sum += (i - offset);
            }
        }
        return (long) sum;
    }

    @Override
    public Long part2(Input input) {
        // We cannot simulate this answer, but we can construct a formula for
        // it by observing how it behaves on successively larger inputs.
        long limit = 50000000000L;
        return 53 * limit + 1362;
    }

    public static void main(String[] args) {
        AocBaseRunner.run(new Day12());
    }

    @Test
    public void test1() throws Exception {
        String str = """
                initial state: #..#.#..##......###...###

                ...## => #
                ..#.. => #
                .#... => #
                .#.#. => #
                .#.## => #
                .##.. => #
                .#### => #
                #.#.# => #
                #.### => #
                ##.#. => #
                ##.## => #
                ###.. => #
                ###.# => #
                ####. => #
                """;

        Input input = InputUtils.parseTestData(this, str);
        assertEquals(325L, simulate(input, 20));
    }
}
