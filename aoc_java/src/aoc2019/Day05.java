package aoc2019;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;

import java.io.IOException;

import org.junit.Test;

import common.AocPuzzle;

/**
 * Day 5: Sunny with a Chance of Asteroids
 *
 * (Contination of the Day 2 IntCode puzzle)
 */
public class Day05 extends AocPuzzle {

    // Original instructions
    static final int OP_ADD = 1;
    static final int OP_MUL = 2;
    static final int OP_END = 99;

    // Added in part 1
    static final int OP_INPUT = 3;
    static final int OP_OUTPUT = 4;
    static final int MODE_POS = 0; // Addressing mode: positional
    static final int MODE_IMM = 1; // Addressing mode: immediate

    // Added in part 2
    static final int OP_JUMP_IF_TRUE = 5;
    static final int OP_JUMP_IF_FALSE = 6;
    static final int OP_LESS_THAN = 7;
    static final int OP_EQUALS = 8;

    boolean part1 = true;

    public Day05() throws IOException {
        super(2019, 5);
    }

    /**
     * Read a parameter from memory.
     *
     * @param prog  The program
     * @param param The parameter value
     * @param mode  The addressing mode, MODE_POS or MODE_IMM.
     * @return
     */
    private int r(int[] prog, int param, int mode) {
        return (mode == MODE_POS) ? prog[param] : param;
    }

    /**
     * Executes the given Intcode program. Returns the possibly modified
     * program.
     *
     * @param program The program.
     * @param input   Sent to the input instruction.
     * @return The output of the last output instruction.
     */
    private int execute(int[] p, int input, boolean diagnostics) {
        int pc = 0;
        int output = 0;

        while (true) {
            int op0 = p[pc] % 100;
            int m1 = (p[pc] / 100) % 10;
            int m2 = (p[pc] / 1000) % 10;

            if (op0 == OP_END)
                return output;

            int op1 = p[pc + 1];

            switch (op0) {
            case OP_ADD: {
                int op2 = p[pc + 2];
                int op3 = p[pc + 3];
                p[op3] = r(p, op1, m1) + r(p, op2, m2);
                pc += 4;
                break;
            }
            case OP_MUL: {
                int op2 = p[pc + 2];
                int op3 = p[pc + 3];
                p[op3] = r(p, op1, m1) * r(p, op2, m2);
                pc += 4;
                break;
            }
            case OP_JUMP_IF_TRUE: {
                int op2 = p[pc + 2];
                pc = (r(p, op1, m1) != 0) ? r(p, op2, m2) : pc + 3;
                break;
            }
            case OP_JUMP_IF_FALSE: {
                int op2 = p[pc + 2];
                pc = (r(p, op1, m1) == 0) ? r(p, op2, m2) : pc + 3;
                break;
            }
            case OP_LESS_THAN: {
                int op2 = p[pc + 2];
                int op3 = p[pc + 3];
                p[op3] = (r(p, op1, m1) < r(p, op2, m2)) ? 1 : 0;
                pc += 4;
                break;
            }
            case OP_EQUALS: {
                int op2 = p[pc + 2];
                int op3 = p[pc + 3];
                p[op3] = (r(p, op1, m1) == r(p, op2, m2)) ? 1 : 0;
                pc += 4;
                break;
            }
            case OP_INPUT: {
                p[op1] = input;
                pc += 2;
                break;
            }
            case OP_OUTPUT: {
                // All but the very last output must be 0.
                assertEquals(0, output);
                output = r(p, op1, m1);
                pc += 2;
                break;
            }
            default:
                fail();
                break;
            }
        }
    }

    int[] parse() throws IOException {
        return parse(getInputAsString());
    }

    int[] parse(String input) {
        String[] s = input.trim().split(",");
        int[] prog = new int[s.length];
        for (int i = 0; i < prog.length; i++) {
            prog[i] = Integer.valueOf(s[i]);
        }
        return prog;
    }

    /*
     * Tests
     */

    @Test
    public void testPart1() throws Exception {
        int[] prog = parse();
        assertEquals(16348437, execute(prog, 1, true));
    }

    @Test
    public void testExample1() {
        int[] prog = parse("3,0,4,0,99");
        int x = 42;
        assertEquals(x, execute(prog, x, true));
        assertArrayEquals(new int[] { x, 0, 4, 0, 99 }, prog);
    }

    @Test
    public void testExample2() {
        int[] prog = parse("1002,4,3,4,33");
        assertEquals(0, execute(prog, 0, true));
        assertArrayEquals(new int[] { 1002, 4, 3, 4, 99 }, prog);
    }

    @Test
    public void testPart2() throws Exception {
        part1 = false;
        int[] prog = parse();
        assertEquals(6959377, execute(prog, 5, false));
    }

    @Test
    public void testExample3() throws Exception {
        part1 = false;
        // Tests for "equal to 8"
        String progstr = "3,9,8,9,10,9,4,9,99,-1,8";
        assertEquals(0, execute(parse(progstr), 1, false));
        assertEquals(1, execute(parse(progstr), 8, false));
    }

    @Test
    public void testExample4() throws Exception {
        part1 = false;
        // Tests for "less than 8"
        String progstr = "3,9,7,9,10,9,4,9,99,-1,8";
        assertEquals(1, execute(parse(progstr), 1, false));
        assertEquals(0, execute(parse(progstr), 8, false));
        assertEquals(0, execute(parse(progstr), 9, false));
    }

    @Test
    public void testExample5() throws Exception {
        part1 = false;
        // Tests for "equal to zero"
        String progstr = "3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9";
        assertEquals(0, execute(parse(progstr), 0, false));
        assertEquals(1, execute(parse(progstr), 42, false));
    }

    @Test
    public void testExample6() throws Exception {
        part1 = false;
        String progstr = "3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,"
                + "1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,"
                + "999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99";
        // The above example program uses an input instruction to ask for a
        // single number. The program will then output 999 if the input value is
        // below 8, output 1000 if the input value is equal to 8, or output 1001
        // if the input value is greater than 8.
        assertEquals(999, execute(parse(progstr), 7, false));
        assertEquals(1000, execute(parse(progstr), 8, false));
        assertEquals(1001, execute(parse(progstr), 9, false));
    }

}
