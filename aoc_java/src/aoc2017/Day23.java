package aoc2017;

import java.io.File;
import java.io.IOException;
import java.math.BigInteger;
import java.util.Optional;

import common2.AocBaseRunner;
import common2.AocPuzzleInfo;
import common2.AocResult;
import common2.IAocIntPuzzle;
import common2.InputUtils;

/**
 * Assembly optimization puzzle. The assembly code in the input is really a
 * program which counts the number of non-primes in an interval. Instead of
 * "interpreting" the assembly code, we just count the (non-)primes.
 * 
 * See
 * https://www.reddit.com/r/adventofcode/comments/7lms6p/2017_day_23_solutions/
 * for explanations for how this works.
 */
public class Day23 implements IAocIntPuzzle<Integer> {

    @Override
    public AocPuzzleInfo getInfo() {
        return new AocPuzzleInfo(2017, 23, "Coprocessor Conflagration", true);
    }

    @Override
    public AocResult<Integer, Integer> getExpected() {
        return AocResult.of(6724, 903);
    }

    /**
     * By reading the assignment of "b" from the actual input file, this
     * solution will (or "should") work for any input.
     */
    @Override
    public Integer parse(Optional<File> file) throws IOException {
        return InputUtils.withReader(file.get(), reader -> {
            try {
                int x = Integer.parseInt(reader.readLine().split(" ")[2]);
                return x;
            } catch (IOException e) {
                throw new RuntimeException(e);
            }
        });
    }

    @Override
    public Integer part1(Integer b) {
        return (b - 2) * (b - 2);
    }

    @Override
    public Integer part2(Integer b) {
        int count = 0;
        int start = (b * 100) + 100000;
        int end = start + 17000;

        for (int i = start; i <= end; i += 17) {
            if (!BigInteger.valueOf(i).isProbablePrime(1)) {
                count++;
            }
        }

        return count;
    }

    public static void main(String[] args) {
        AocBaseRunner.run(new Day23());
    }
}
