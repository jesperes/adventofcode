package aoc2018;

import static org.junit.Assert.assertEquals;

import java.io.File;
import java.io.IOException;
import java.util.Arrays;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

import org.junit.Test;

import common2.AocBaseRunner;
import common2.AocPuzzleInfo;
import common2.AocResult;
import common2.IAocIntPuzzle;
import common2.InputUtils;

/**
 * The runtime of this puzzle is dominated by parsing, but the list of integers
 * is >64k which means that we cannot easily keep it inline in the code either.
 */
public class Day08 implements IAocIntPuzzle<List<Integer>> {

    @Override
    public AocPuzzleInfo getInfo() {
        return new AocPuzzleInfo(2018, 8, "Memory Maneuver", true);
    }

    @Override
    public AocResult<Integer, Integer> getExpected() {
        return AocResult.of(36307, 25154);
    }

    @Override
    public List<Integer> parse(Optional<File> file) throws IOException {
        return Arrays.stream(InputUtils.asString(file.get()).split(" "))
                .map(s -> Integer.parseInt(s))
                .collect(Collectors.toUnmodifiableList());
    }

    @Override
    public Integer part1(List<Integer> input) {
        return getMetadataSum(input, 0).sum;
    }

    record MetadataSum(int sum, int nexti) {
    }

    MetadataSum getMetadataSum(List<Integer> input, int i) {
        int childNodes = input.get(i);
        int numMetadataEntries = input.get(i + 1);
        int sum = 0;
        int nexti = i + 2;
        for (int j = 0; j < childNodes; j++) {
            var metadataSum = getMetadataSum(input, nexti);
            sum += metadataSum.sum;
            nexti = metadataSum.nexti;
        }

        for (int j = 0; j < numMetadataEntries; j++) {
            sum += input.get(nexti++);
        }

        return new MetadataSum(sum, nexti);
    }

    @Override
    public Integer part2(List<Integer> input) {
        return valueOf(input, 0).value;
    }

    record Value(int value, int nexti) {
    }

    private Value valueOf(List<Integer> input, int i) {
        int childNodes = input.get(i);
        int numMetadataEntries = input.get(i + 1);

        int nexti = i + 2;
        int[] childValues = new int[childNodes];
        for (int j = 0; j < childNodes; j++) {
            var v = valueOf(input, nexti);
            childValues[j] = v.value;
            nexti = v.nexti;
        }

        int value = 0;
        for (int j = 0; j < numMetadataEntries; j++) {
            if (childNodes == 0) {
                value += input.get(nexti++);
            } else {
                int metadataIdx = input.get(nexti++) - 1;
                if (metadataIdx < 0 || metadataIdx >= childNodes) {
                    continue;
                } else {
                    value += childValues[metadataIdx];
                }
            }
        }
        return new Value(value, nexti);
    }

    @Test
    public void testName() throws Exception {
        assertEquals(138, (int) part1(
                List.of(2, 3, 0, 3, 10, 11, 12, 1, 1, 0, 1, 99, 2, 1, 1, 2)));
    }

    public static void main(String[] args) {
        AocBaseRunner.run(new Day08());
    }
}
