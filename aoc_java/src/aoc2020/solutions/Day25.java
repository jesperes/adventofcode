package aoc2020.solutions;

import static org.junit.Assert.assertEquals;

import java.io.File;
import java.io.FileNotFoundException;
import java.math.BigInteger;
import java.util.Optional;

import org.junit.Test;

import aoc2020.AocBaseRunner;
import aoc2020.AocPuzzleInfo;
import aoc2020.AocResult;
import aoc2020.IAocPuzzle;
import aoc2020.solutions.Day25.PublicKeys;

public class Day25 implements IAocPuzzle<PublicKeys, BigInteger, Void> {

    static final BigInteger NUM = BigInteger.valueOf(20201227);

    record PublicKeys(BigInteger card, BigInteger door) {
    }

    @Override
    public AocPuzzleInfo getInfo() {
        return new AocPuzzleInfo(2020, 25, "Combo Breaker", false);
    }

    @Override
    public AocResult<BigInteger, Void> getExpected() {
        return AocResult.of(BigInteger.valueOf(1890859L), null);
    }

    @Override
    public PublicKeys parse(Optional<File> file) {
        return new PublicKeys(BigInteger.valueOf(15113849L),
                BigInteger.valueOf(4206373L));
    }

    @Override
    public BigInteger part1(PublicKeys keys) {
        return keys.card().modPow(findLoopSize(keys.door), NUM);
    }

    private BigInteger findLoopSize(BigInteger door) {
        var subject = BigInteger.valueOf(7L);
        var current = subject;

        for (long n = 1;; n++) {
            BigInteger pk0 = current.multiply(subject).mod(NUM);
            if (pk0.equals(door)) {
                return BigInteger.valueOf(n + 1);
            } else {
                current = pk0;
            }
        }
    }

    @Override
    public Void part2(PublicKeys input) {
        return null;
    }

    public static void main(String[] args) throws FileNotFoundException {
        AocBaseRunner.run(new Day25());
    }

    @Test
    public void testFindLoopSize() throws Exception {
        assertEquals(11L, findLoopSize(BigInteger.valueOf(17807724L)));
        assertEquals(8L, findLoopSize(BigInteger.valueOf(5764801L)));
    }
}
