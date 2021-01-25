package aoc2017;

import java.io.File;
import java.io.IOException;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Optional;
import java.util.Set;

import common2.AocBaseRunner;
import common2.AocPuzzleInfo;
import common2.AocResult;
import common2.IAocIntPuzzle;

public class Day06 implements IAocIntPuzzle<int[]> {

    private int getStartBank(int[] banks) {
        int max = Integer.MIN_VALUE;
        int startBank = -1;
        for (int i = 0; i < banks.length; i++) {
            if (banks[i] > max) {
                startBank = i;
                max = banks[i];
            }
        }
        return startBank;
    }

    private void redistribute(int[] banks) {
        int bankIdx = getStartBank(banks);
        int bankSize = banks[bankIdx];

        // Empty the start bank
        banks[bankIdx] = 0;

        // Distribute the memory blocks one-by-one, starting with
        // the bank next to the start bank.
        while (bankSize > 0) {
            bankIdx = (bankIdx + 1) % banks.length;
            banks[bankIdx]++;
            bankSize--;
        }
    }

    private int redistribute1(int[] banks) {
        Set<String> seenBanks = new HashSet<>();
        int passes = 0;
        while (true) {
            /*
             * Store arrays as strings, so that we can put them in a set. We
             * can't put the arrays themselves into the set because they do not
             * have proper hashCode/equals semantics (different arrays do not
             * compare equal).
             */
            String str = Arrays.toString(banks);

            if (seenBanks.contains(str)) {
                return passes;
            } else {
                seenBanks.add(str);
                redistribute(banks);
                passes++;
            }
        }
    }

    private int redistribute2(int[] banks) {
        /*
         * Instead of a set, we keep a map from the array (as a string). As
         * value we put the pass when we saw the configuration. This allows us
         * to compute the size of the loop.
         */
        Map<String, Integer> seenBanks = new HashMap<>();
        int passes = 0;
        while (true) {
            String str = Arrays.toString(banks);

            if (seenBanks.containsKey(str)) {
                int firstSeenPass = seenBanks.get(str);
                return passes - firstSeenPass;
            } else {
                seenBanks.put(str, passes);
                redistribute(banks);
                passes++;
            }
        }
    }

    @Override
    public AocPuzzleInfo getInfo() {
        return new AocPuzzleInfo(2017, 6, "Memory Reallocation", false);
    }

    @Override
    public AocResult<Integer, Integer> getExpected() {
        return AocResult.of(3156, 1610);
    }

    @Override
    public int[] parse(Optional<File> file) throws IOException {
        return new int[] { 2, 8, 8, 5, 4, 2, 3, 1, 5, 5, 1, 2, 15, 13, 5, 14 };
    }

    @Override
    public Integer part1(int[] input) {
        return redistribute1(input);
    }

    @Override
    public Integer part2(int[] input) {
        return redistribute2(input);
    }

    public static void main(String[] args) {
        AocBaseRunner.run(new Day06());
    }
}
