package aoc2017;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.util.stream.IntStream;

import org.junit.Test;

/**
 * --- Day 15: Dueling Generators ---
 *
 * Here, you encounter a pair of dueling generators. The generators, called
 * generator A and generator B, are trying to agree on a sequence of numbers.
 * However, one of them is malfunctioning, and so the sequences don't always
 * match.
 *
 * As they do this, a judge waits for each of them to generate its next value,
 * compares the lowest 16 bits of both values, and keeps track of the number of
 * times those parts of the values match.
 *
 * The generators both work on the same principle. To create its next value, a
 * generator will take the previous value it produced, multiply it by a factor
 * (generator A uses 16807; generator B uses 48271), and then keep the remainder
 * of dividing that resulting product by 2147483647. That final remainder is the
 * value it produces next.
 *
 * To calculate each generator's first value, it instead uses a specific
 * starting value as its "previous value" (as listed in your puzzle input).
 *
 * For example, suppose that for starting values, generator A uses 65, while
 * generator B uses 8921. Then, the first five pairs of generated values are:
 *
 * --Gen. A-- --Gen. B--
 *
 * 1092455 430625591
 *
 * 1181022009 1233683848
 *
 * 245556042 1431495498
 *
 * 1744312007 137874439
 *
 * 1352636452 285222916
 *
 * In binary, these pairs are (with generator A's value first in each pair):
 *
 * 00000000000100001010101101100111 00011001101010101101001100110111
 *
 * 01000110011001001111011100111001 01001001100010001000010110001000
 *
 * 00001110101000101110001101001010 01010101010100101110001101001010
 *
 * 01100111111110000001011011000111 00001000001101111100110000000111
 *
 * 01010000100111111001100000100100 00010001000000000010100000000100
 *
 * Here, you can see that the lowest (here, rightmost) 16 bits of the third
 * value match: 1110001101001010. Because of this one match, after processing
 * these five pairs, the judge would have added only 1 to its total.
 *
 * To get a significant sample, the judge would like to consider 40 million
 * pairs. (In the example above, the judge would eventually find a total of 588
 * pairs that match in their lowest 16 bits.)
 *
 * After 40 million pairs, what is the judge's final count?
 *
 * @author jespe
 *
 */

public class Day15 {

    static class Generator {
        final long factor;
        final long initialValue;
        static private final long mod = 2147483647L;
        long value;

        public Generator(long factor, long value) {
            this.initialValue = value;
            this.factor = factor;
            this.value = value;
        }

        public void reset() {
            value = initialValue;
        }

        public long getValue() {
            return value;
        }

        public long generateNextValue() {
            value = (value * factor) % mod;
            return value;
        }
    }

    static class Generator_Part2 extends Generator {

        private long multiple;

        public Generator_Part2(long factor, long value, long multiple) {
            super(factor, value);
            this.multiple = multiple;
        }

        @Override
        public long generateNextValue() {
            while (true) {
                long nextValue = super.generateNextValue();
                if (nextValue % multiple == 0)
                    return nextValue;
            }
        }
    }

    static class GeneratorA extends Generator {
        public GeneratorA(long value) {
            super(16807, value);
        }
    }

    static class GeneratorB extends Generator {
        public GeneratorB(long value) {
            super(48271, value);
        }
    }

    static class GeneratorA_Part2 extends Generator_Part2 {
        public GeneratorA_Part2(long value) {
            super(16807, value, 4);
        }
    }

    static class GeneratorB_Part2 extends Generator_Part2 {
        public GeneratorB_Part2(long value) {
            super(48271, value, 8);
        }
    }

    long A = 277;
    long B = 349;

    /**
     * Return true iff a and b are identical in their 16 lowest bits.
     *
     * @param a
     * @param b
     * @return
     */
    static boolean equalsLower16(long a, long b) {
        return (a & 0xffff) == (b & 0xffff);
    }

    @Test
    public void testGenerateValue() throws Exception {
        Generator genA = new GeneratorA(65);
        Generator genB = new GeneratorB(8921);

        assertEquals(1092455L, genA.generateNextValue());
        assertEquals(1181022009L, genA.generateNextValue());
        assertEquals(245556042L, genA.generateNextValue());
        assertEquals(1744312007L, genA.generateNextValue());
        assertEquals(1352636452, genA.generateNextValue());

        assertEquals(430625591L, genB.generateNextValue());
        assertEquals(1233683848L, genB.generateNextValue());
        assertEquals(1431495498L, genB.generateNextValue());
        assertEquals(137874439L, genB.generateNextValue());
        assertEquals(285222916L, genB.generateNextValue());
    }

    @Test
    public void testGenerateValue_Part2() throws Exception {
        Generator genA = new GeneratorA_Part2(65);
        Generator genB = new GeneratorB_Part2(8921);

        assertEquals(1352636452L, genA.generateNextValue());
        assertEquals(1992081072L, genA.generateNextValue());
        assertEquals(530830436L, genA.generateNextValue());
        assertEquals(1980017072L, genA.generateNextValue());
        assertEquals(740335192L, genA.generateNextValue());

        assertEquals(1233683848L, genB.generateNextValue());
        assertEquals(862516352L, genB.generateNextValue());
        assertEquals(1159784568L, genB.generateNextValue());
        assertEquals(1616057672L, genB.generateNextValue());
        assertEquals(412269392L, genB.generateNextValue());
    }

    @Test
    public void testCompare16() throws Exception {

        Generator genA = new GeneratorA(65);
        Generator genB = new GeneratorB(8921);
        long valueA;
        long valueB;

        valueA = genA.generateNextValue();
        valueB = genB.generateNextValue();
        assertFalse(equalsLower16(valueA, valueB));

        valueA = genA.generateNextValue();
        valueB = genB.generateNextValue();
        assertFalse(equalsLower16(valueA, valueB));

        // According to the spec, the third pair of numbers should be equal in
        // their lower 16 bits.
        valueA = genA.generateNextValue();
        valueB = genB.generateNextValue();
        assertTrue(equalsLower16(valueA, valueB));

        valueA = genA.generateNextValue();
        valueB = genB.generateNextValue();
        assertFalse(equalsLower16(valueA, valueB));

        valueA = genA.generateNextValue();
        valueB = genB.generateNextValue();
        assertFalse(equalsLower16(valueA, valueB));

    }

    @Test
    public void testCompareValues1() throws Exception {
        Generator genA = new GeneratorA(65);
        Generator genB = new GeneratorB(8921);

        long matches = IntStream.range(0, 5)
                .filter(n -> equalsLower16(genA.generateNextValue(),
                        genB.generateNextValue()))
                .count();

        // According to the spec, in the first 5 numbers, there should be
        // exactly one (the third) pair of numbers which match in their lowest
        // 16 bits.
        assertEquals(1, matches);

        genA.reset();
        genB.reset();

        matches = IntStream.range(0, 40_000_000)
                .filter(n -> equalsLower16(genA.generateNextValue(),
                        genB.generateNextValue()))
                .count();

        // There should be exactly 588 pairs in the first 40 million pairs.
        assertEquals(588, matches);
    }

    @Test
    public void testCompareValues_Full() throws Exception {
        Generator genA = new GeneratorA(277);
        Generator genB = new GeneratorB(349);

        // Consider the first 40 million pairs.
        long matches = IntStream.range(0, 40_000_000)
                .filter(n -> equalsLower16(genA.generateNextValue(),
                        genB.generateNextValue()))
                .count();

        assertEquals(592, matches);
    }

    @Test
    public void testCompareValues_Part2() throws Exception {
        Generator genA = new GeneratorA_Part2(65);
        Generator genB = new GeneratorB_Part2(8921);

        // First match comes at pair number 1056.
        long matches = IntStream.range(0, 1056)
                .filter(n -> equalsLower16(genA.generateNextValue(),
                        genB.generateNextValue()))
                .count();

        assertEquals(1, matches);

        genA.reset();
        genB.reset();

        matches = IntStream.range(0, 5_000_000)
                .filter(n -> equalsLower16(genA.generateNextValue(),
                        genB.generateNextValue()))
                .count();

        // There should be exactly 309 pairs in the first 5 million pairs.
        assertEquals(309, matches);
    }

    @Test
    public void testCompareValuesFull_Part2() throws Exception {
        Generator genA = new GeneratorA_Part2(277);
        Generator genB = new GeneratorB_Part2(349);

        long matches = IntStream.range(0, 5_000_000)
                .filter(n -> equalsLower16(genA.generateNextValue(),
                        genB.generateNextValue()))
                .count();

        assertEquals(320, matches);
    }
}
