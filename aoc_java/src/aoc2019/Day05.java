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

    static boolean part1 = true;

    public Day05() throws IOException {
        super(2019, 5);
    }

    int read(int[] prog, int param, int mode) {
        if (mode == MODE_POS) {
            return prog[param];
        } else {
            return param;
        }
    }

    /**
     * Executes the given Intcode program. Returns the possibly modified
     * program.
     * 
     * @param program The program.
     * @param input   Sent to the input instruction.
     * @return The output of the last output instruction.
     */
    private int executeIntCode(int[] prog, int input) {
        int pc = 0;
        int cntr = 0;
        int output = 0;

        while (true) {
            int op0 = prog[pc] % 100;
            int mode1 = (prog[pc] / 100) % 10;
            int mode2 = (prog[pc] / 1000) % 10;
            cntr++;

            if (op0 == OP_END)
                return output;

            switch (op0) {
            case OP_ADD: {
                int op1 = prog[pc + 1];
                int op2 = prog[pc + 2];
                int op3 = prog[pc + 3];
                prog[op3] = read(prog, op1, mode1) + read(prog, op2, mode2);
                pc += 4;
                break;
            }
            case OP_MUL: {
                int op1 = prog[pc + 1];
                int op2 = prog[pc + 2];
                int op3 = prog[pc + 3];
                prog[op3] = read(prog, op1, mode1) * read(prog, op2, mode2);
                pc += 4;
                break;
            }
            case OP_JUMP_IF_TRUE: {
                int op1 = prog[pc + 1];
                int op2 = prog[pc + 2];
                if (read(prog, op1, mode1) != 0) {
                    pc = read(prog, op2, mode2);
                } else {
                    pc += 3;
                }
                break;
            }
            case OP_JUMP_IF_FALSE: {
                int op1 = prog[pc + 1];
                int op2 = prog[pc + 2];
                if (read(prog, op1, mode1) == 0) {
                    pc = read(prog, op2, mode2);
                } else {
                    pc += 3;
                }
                break;
            }
            case OP_LESS_THAN: {
                // Opcode 7 is less than: if the first parameter is less than
                // the second parameter, it stores 1 in the position given by
                // the third parameter. Otherwise, it stores 0.
                int op1 = prog[pc + 1];
                int op2 = prog[pc + 2];
                int op3 = prog[pc + 3];

                prog[op3] = (read(prog, op1, mode1) < read(prog, op2, mode2))
                        ? 1
                        : 0;
                pc += 4;
                break;
            }
            case OP_EQUALS: {
                int op1 = prog[pc + 1];
                int op2 = prog[pc + 2];
                int op3 = prog[pc + 3];

                prog[op3] = (read(prog, op1, mode1) == read(prog, op2, mode2))
                        ? 1
                        : 0;
                pc += 4;
                break;
            }
            case OP_INPUT: {
                int op1 = prog[pc + 1];
                prog[op1] = input;
                pc += 2;
                break;
            }
            case OP_OUTPUT: {
                int op1 = prog[pc + 1];
                output = read(prog, op1, mode1);
                // System.out.println("OUTPUT: " + output);
                if (part1) {
                    if (prog[pc + 2] % 100 == OP_END) {
                        return output;
                    } else {
                        assertEquals(0, output);
                        pc += 2;
                        break;
                    }
                } else {
                    return output;
                }
                // break;
            }
            default:
                fail();
            }

        }

    }

    int[] parseIntCode() throws IOException {
        return parseIntCode(getInputAsString());
    }

    int[] parseIntCode(String input) {
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
        int[] prog = parseIntCode();
        assertEquals(16348437, executeIntCode(prog, 1));
    }

    @Test
    public void testPart2() throws Exception {
        part1 = false;
        int[] prog = parseIntCode();
        assertEquals(6959377, executeIntCode(prog, 5));
    }

    @Test
    public void testExample1() {
        int[] prog = parseIntCode("3,0,4,0,99");
        int x = 42;
        assertEquals(x, executeIntCode(prog, x));
        assertArrayEquals(new int[] { x, 0, 4, 0, 99 }, prog);
    }

    @Test
    public void testExample2() {
        int[] prog = parseIntCode("1002,4,3,4,33");
        assertEquals(0, executeIntCode(prog, 0));
        assertArrayEquals(new int[] { 1002, 4, 3, 4, 99 }, prog);
    }

    @Test
    public void testExample3() throws Exception {
        part1 = false;
        // Tests for "equal to 8"
        String progstr = "3,9,8,9,10,9,4,9,99,-1,8";
        assertEquals(0, executeIntCode(parseIntCode(progstr), 1)); // 1 != 8
        assertEquals(1, executeIntCode(parseIntCode(progstr), 8)); // 1 == 8
    }

    @Test
    public void testExample4() throws Exception {
        part1 = false;
        // Tests for "less than 8"
        String progstr = "3,9,7,9,10,9,4,9,99,-1,8";
        assertEquals(1, executeIntCode(parseIntCode(progstr), 1)); // 1 < 8
        assertEquals(0, executeIntCode(parseIntCode(progstr), 8)); // !(8 < 8)
        assertEquals(0, executeIntCode(parseIntCode(progstr), 9)); // !(9 < 8)
    }

    @Test
    public void testExample5() throws Exception {
        part1 = false;
        // Tests for "equal to zero"
        String progstr = "3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9";
        assertEquals(0, executeIntCode(parseIntCode(progstr), 0)); // 0 == 0
        assertEquals(1, executeIntCode(parseIntCode(progstr), 42)); // 42 != 0
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
        assertEquals(999, executeIntCode(parseIntCode(progstr), 7));
        assertEquals(1000, executeIntCode(parseIntCode(progstr), 8));
        assertEquals(1001, executeIntCode(parseIntCode(progstr), 9));
    }

}
