package aoc2020.solutions;

import static java.lang.Long.parseLong;
import static java.util.Arrays.stream;
import static java.util.stream.Collectors.toList;
import static java.util.stream.IntStream.range;
import static org.junit.Assert.assertEquals;

import java.io.File;
import java.io.FileNotFoundException;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.stream.Collectors;

import org.junit.Test;

import aoc2020.AocBaseRunner;
import aoc2020.AocPuzzleInfo;
import aoc2020.AocResult;
import aoc2020.IAocIntPuzzle;
import aoc2020.InputUtils;
import aoc2020.solutions.Day14.Writes;

public class Day14 implements IAocIntPuzzle<List<Writes>> {

    record Write(long address, long value) {

    }

    record Writes(String mask, List<Write> writes) {

    }

    @Override
    public AocPuzzleInfo getInfo() {
        return new AocPuzzleInfo(2020, 14, "Docking Data", true);
    }

    @Override
    public AocResult<Long, Long> getExpected() {
        return AocResult.of(8471403462063L, 2667858637669L);
    }

    @Override
    public List<Writes> parse(Optional<File> file) {
        return stream(InputUtils.asString(file.get()).split("mask = "))
                .filter(s -> s.length() > 0).map(block -> {
                    String[] lines = block.split("\n");
                    return new Writes(lines[0],
                            stream(lines).skip(1).map(line -> {
                                String[] elems = line.split("[\\[\\] =]+");
                                return new Write(parseLong(elems[1]),
                                        parseLong(elems[2]));
                            }).collect(toList()));
                }).collect(toList());
    }

    @Override
    public Long part1(List<Writes> input) {
        Map<Long, Long> memory = new HashMap<>();
        for (Writes writes : input) {
            String mask = writes.mask();
            for (Write w : writes.writes) {
                long maskedValue = w.value;
                for (int i = 0; i < mask.length(); i++) {
                    char c = mask.charAt(i);
                    int pos = 35 - i;
                    if (c == 'X')
                        continue;
                    else if (c == '1')
                        maskedValue |= (1L << pos);
                    else if (c == '0')
                        maskedValue &= ~(1L << pos);
                    else
                        throw new RuntimeException();
                }
                memory.put(w.address, maskedValue);
            }
        }
        return memory.values().stream().collect(Collectors.summingLong(n -> n));
    }

    @Override
    public Long part2(List<Writes> input) {
        Map<Long, Long> memory = new HashMap<>();
        // floatingBits tracks the location of each X
        Map<Integer, Integer> floatingBits = new HashMap<>();

        for (Writes writes : input) {
            String mask = writes.mask();

            floatingBits.clear();
            for (int i = 0, j = 0; i < mask.length(); i++) {
                if (mask.charAt(i) == 'X')
                    floatingBits.put(i, j++);
            }

            for (Write w : writes.writes) {
                /*
                 * Generate all numbers from 0 to 2**(number of X:s). These
                 * numbers contain all possible bit-combinations to use for the
                 * floating bits.
                 */
                range(0, 1 << floatingBits.size()).forEach(floatBitValues -> {
                    long maskedAddr = w.address;
                    for (int i = 0; i < mask.length(); i++) {
                        char c = mask.charAt(i);
                        int pos = 35 - i;
                        if (c == '0') // unchanged
                            continue;
                        else if (c == '1')
                            maskedAddr |= (1L << pos);
                        else if (c == 'X') {
                            // lookup the bit in 'floatBitValues' and use that
                            // to know what we should set the bit in the
                            // address to.
                            int floatPos = floatingBits.get(i);
                            if ((floatBitValues & (1L << floatPos)) == 0) {
                                maskedAddr |= (1L << pos);
                            } else {
                                maskedAddr &= ~(1L << pos);
                            }
                        } else
                            throw new RuntimeException();
                    }
                    memory.put(maskedAddr, w.value);
                });
            }
        }
        return memory.values().stream().collect(Collectors.summingLong(n -> n));
    }

    public static void main(String[] args) throws FileNotFoundException {
        AocBaseRunner.run(new Day14());
    }

    @Test
    public void testP1() throws Exception {
        var w = List.of(new Writes("XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X",
                List.of(new Write(8, 11), new Write(7, 101), new Write(8, 0))));

        assertEquals(165L, (long) part1(w));
    }

    @Test
    public void testP2() throws Exception {
        var w = List.of(
                new Writes("000000000000000000000000000000X1001X",
                        List.of(new Write(42, 100))),
                new Writes("00000000000000000000000000000000X0XX",
                        List.of(new Write(26, 1))));

        assertEquals(208L, (long) part2(w));
    }
}
