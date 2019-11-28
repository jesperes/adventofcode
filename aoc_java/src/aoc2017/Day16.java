package aoc2017;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;

import java.io.IOException;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import org.junit.Before;
import org.junit.Test;

import common.AocPuzzle;

public class Day16 extends AocPuzzle {

    public Day16() {
        super(2017, 16);
    }

    abstract class Instruction {
        abstract public void perform(char[] actor);
    }

    class Spin extends Instruction {
        final private int count;
        char tmp[];

        Spin(int n) {
            count = n;
        }

        public String toString() {
            return "s" + String.valueOf(count);
        }

        @Override
        public void perform(char[] actors) {
            // Allocate temp array
            if (tmp == null)
                tmp = new char[actors.length];

            if (count == 0 || count == actors.length)
                return;

            if (count < 0 || count >= actors.length) {
                throw new AssertionError("invalid count: " + count);
            }

            // Move the last count actors to the temporary array
            for (int i = 0; i < count; i++) {
                tmp[i] = actors[actors.length - count + i];
            }

            // Move actors to the end.
            for (int i = actors.length - 1; i >= count; i--) {
                int src_idx = i - count;
                int dest_idx = i;
                actors[dest_idx] = actors[src_idx];
            }

            // Copy actors back from temp storage.
            for (int i = 0; i < count; i++) {
                actors[i] = tmp[i];
            }
        }
    }

    class Exchange extends Instruction {
        final private int indexa;
        final private int indexb;

        Exchange(int indexa, int indexb) {
            this.indexa = indexa;
            this.indexb = indexb;
        }

        public String toString() {
            return String.format("x%d/%d", indexa, indexb);
        }

        @Override
        public void perform(char[] actors) {
            char tmp = actors[indexa];
            actors[indexa] = actors[indexb];
            actors[indexb] = tmp;
        }
    }

    class Partner extends Instruction {
        final private char proga;
        final private char progb;

        public Partner(char proga, char progb) {
            this.proga = proga;
            this.progb = progb;
        }

        public String toString() {
            return String.format("p%c/%c", proga, progb);
        }

        @Override
        public void perform(char[] actors) {
            int indexa = -1;
            int indexb = -1;

            for (int i = 0; i < actors.length; i++) {
                if (actors[i] == proga)
                    indexa = i;
                if (actors[i] == progb)
                    indexb = i;

                // We've found both programs, do the swap
                if (indexa >= 0 && indexb >= 0) {
                    char tmp = actors[indexa];
                    actors[indexa] = actors[indexb];
                    actors[indexb] = tmp;
                    return;
                }
            }
        }
    }

    List<Instruction> fullInput;
    char fullActors[] = "abcdefghijklmnop".toCharArray();

    private List<Instruction> parseInstructions(String str) {
        return Arrays.stream(str.split(",")).map(instr -> {
            switch (instr.charAt(0)) {
            case 's': // Spin
                return new Spin(Integer.valueOf(instr.substring(1)));
            case 'x': // Exchange
            {
                String elems[] = instr.substring(1).split("/");
                return new Exchange(Integer.valueOf(elems[0]),
                        Integer.valueOf(elems[1]));
            }
            case 'p': // Partner
                return new Partner(instr.charAt(1), instr.charAt(3));
            default:
                fail("unknown move " + instr.charAt(0));
                return null;
            }
        }).collect(Collectors.toList());
    }

    public void performDance(char[] a, List<Instruction> instructions) {
        for (Instruction instr : instructions) {
            instr.perform(a);
        }
    }

    @Before
    public void setup() throws IOException {
        fullInput = parseInstructions(getInputAsString());
    }

    @Test
    public void testInstructionParser() throws Exception {
        String instructionString = getInputAsString();

        List<Instruction> instructions = parseInstructions(instructionString);

        String instructionString2 = instructions.stream()
                .map(instr -> instr.toString())
                .collect(Collectors.joining(","));

        assertEquals(instructionString2, instructionString);
    }

    @Test
    public void testSpin() throws Exception {
        {
            char actors[] = "abcde".toCharArray();
            performDance(actors, parseInstructions("s0"));
            assertArrayEquals("abcde".toCharArray(), actors);
        }

        {
            char actors[] = "abcde".toCharArray();
            performDance(actors, parseInstructions("s3"));
            assertArrayEquals("cdeab".toCharArray(), actors);
        }

        {
            char actors[] = "abcde".toCharArray();
            performDance(actors, parseInstructions("s1"));
            assertArrayEquals("eabcd".toCharArray(), actors);
        }
    }

    @Test
    public void testExchange() throws Exception {
        char actors[] = "abcde".toCharArray();
        performDance(actors, parseInstructions("x3/4"));
        assertArrayEquals("abced".toCharArray(), actors);
    }

    @Test
    public void testPartner() throws Exception {
        {
            char actors[] = "abcde".toCharArray();
            performDance(actors, parseInstructions("pe/c"));
            assertArrayEquals("abedc".toCharArray(), actors);
        }

        {
            char actors[] = "abcde".toCharArray();
            performDance(actors, parseInstructions("pa/b"));
            assertArrayEquals("bacde".toCharArray(), actors);
        }
    }

    @Test
    public void testInputSmall() throws Exception {
        char[] actors = "abcde".toCharArray();
        performDance(actors, parseInstructions("s1,x3/4,pe/b"));
        assertArrayEquals("baedc".toCharArray(), actors);
    }

    @Test
    public void testInputFull() throws Exception {
        performDance(fullActors, fullInput);
        assertArrayEquals("kbednhopmfcjilag".toCharArray(), fullActors);
    }

    @Test
    public void testInputFull_Part2() throws Exception {
        /*
         * We optimize this by caching known results, and only perform dances
         * for input configurations we haven't seen before. This cuts the time
         * per iteration from ~300000ns to ~15ns.
         * 
         * This takes the runtime down to 8s which is doable.
         */
        Map<String, String> map = new HashMap<>();
        int repeat = 1_000_000_000;

        String state = new String(fullActors);

        for (int i = 0; i < repeat; i++) {
            String newState = map.getOrDefault(state, null);

            if (newState == null) {
                fullActors = state.toCharArray();
                performDance(fullActors, fullInput);
                map.put(state, new String(fullActors));
            } else {
                state = newState;
            }
        }

        assertEquals("fbmcgdnjakpioelh", state);
    }
}
