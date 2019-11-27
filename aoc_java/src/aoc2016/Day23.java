package aoc2016;

import static org.junit.Assert.assertEquals;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;

import org.junit.Test;

public class Day23 {

    static String[] parseProgram(String filename) throws IOException {
        try (BufferedReader r = new BufferedReader(new FileReader(filename))) {
            return r.lines().toArray(n -> new String[n]);
        }
    }

    public static void main(String[] args) throws IOException {
        String[] prog = parseProgram("input.txt");
        int[] regs = new int[4];
        interpret(prog, 0, regs);
    }

    static void writeReg(String reg, int[] regs, int value) {
        regs[reg.charAt(0) - 'a'] = value;
    }

    static int readArg(String arg, int[] regs) {
        if (isReg(arg))
            return regs[arg.charAt(0) - 'a'];
        else
            return Integer.valueOf(arg);
    }

    static boolean isReg(String s) {
        char c = s.charAt(0);
        return (c >= 'a' && c <= 'd');
    }

    static void interpret(String[] prog, int pc, int[] regs) {
        while (pc < prog.length) {
            String line = prog[pc];
            String[] ops = line.split(" ");

            // System.out.format("pc = %10d: instr = %20s, regs = %s%n", pc,
            // line,
            // Arrays.toString(regs));

            switch (ops[0]) {
            case "cpy": // 2 args
                if (isReg(ops[2]))
                    writeReg(ops[2], regs, readArg(ops[1], regs));
                pc++;
                break;
            case "mul": // 3 args
                writeReg(ops[3], regs,
                        readArg(ops[1], regs) * readArg(ops[2], regs));
                pc++;
                break;
            case "inc": // 1 args
                if (isReg(ops[1]))
                    writeReg(ops[1], regs, readArg(ops[1], regs) + 1);
                pc++;
                break;
            case "dec": // 1 args
                if (isReg(ops[1]))
                    writeReg(ops[1], regs, readArg(ops[1], regs) - 1);
                pc++;
                break;
            case "jnz": // 2 args
                if (readArg(ops[1], regs) != 0)
                    pc += readArg(ops[2], regs);
                else
                    pc++;
                break;
            case "tgl": // 1 args
                int p = pc + readArg(ops[1], regs);
                if (p < 0 || p >= prog.length) {
                    pc++;
                    break;
                }

                String line0 = prog[p];
                String[] ops0 = line0.split(" ");

                switch (ops0[0]) {
                case "inc":
                    prog[p] = prog[p].replace("inc", "dec");
                    break;
                case "dec":
                    prog[p] = prog[p].replace("dec", "inc");
                    break;
                case "tgl":
                    prog[p] = prog[p].replace("tgl", "inc");
                    break;
                case "jnz":
                    prog[p] = prog[p].replace("jnz", "cpy");
                    break;
                case "cpy":
                    prog[p] = prog[p].replace("cpy", "jnz");
                    break;
                default:
                    throw new RuntimeException();
                }

                pc++;
                break;
            default:
                throw new RuntimeException();
            }
        }
    }

    @Test
    public void testInstr() throws Exception {
        int[] regs = new int[4];

        interpret(new String[] {
                // @formatter:off
				"cpy 41 a", "inc a", "inc a", "dec a", "jnz a 2", "dec a"
				// @formatter:on
        }, 0, regs);

        assertEquals(42, regs[0]);
    }

    @Test
    public void testToggle() throws Exception {
        int[] regs = new int[4];

        interpret(new String[] {
                // @formatter:off
				"cpy 2 a", 
				"tgl a", 
				"tgl a", 
				"tgl a", 
				"cpy 1 a", 
				"dec a", 
				"dec a"
				// @formatter:on
        }, 0, regs);

        assertEquals(3, regs[0]);

    }

    @Test
    public void testPart1() throws Exception {
        int[] regs = new int[4];
        regs[0] = 7;
        interpret(parseProgram("inputs/2016/day23.txt"), 0, regs);
        assertEquals(11130, regs[0]);
    }

    long factorial(long x) {
        return x == 1 ? 1 : x * factorial(x - 1);
    }

    @Test
    public void testPart2() throws Exception {
        /*
         * Part 2 requires us to understand what the program does so that we can
         * optimize it. The program implements factorial using only add, so we
         * can shortcut it by implementing factorial ourselves.
         */
        int offset = 70 * 87;
        int seed = 12;
        assertEquals(479007690, offset + factorial(seed));
    }
}
