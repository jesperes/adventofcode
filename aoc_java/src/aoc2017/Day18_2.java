package aoc2017;

import static org.junit.Assert.assertEquals;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.HashSet;
import java.util.Set;

import org.junit.Test;

import common.AocPuzzle;

public class Day18_2 extends AocPuzzle {
    public Day18_2() {
        super(2017, 18);
    }

    enum Instr {
        set, add, mul, mod, snd, rcv, jgz
    }

    // @formatter:off
	String[] input1 = {
		"set a 1",
		"add a 2",
		"mul a a",
		"mod a 5",
		"snd a",
		"set a 0",
		"rcv a",
		"jgz a -1",
		"set a 1",
		"jgz a -2"
	};
	// @formatter:on

    Set<String> regs = new HashSet<>();

    private boolean isReg(String op) {
        return op.matches("[a-z]");
    }

    private void declareReg(String op, StringBuilder output) {
        if (isReg(op) && !regs.contains(op)) {
            output.append(String.format("  int64_t %s = 0;%n", op));
            regs.add(op);
        }
    }

    private void compile(String[] list, StringBuilder output) {

        // System.out.format("Compiling %d instructions.%n", list.length);
        // System.out.println("-----------------------------------");

        output.append("#include <stdio.h>\n");
        output.append("#include <inttypes.h>\n");
        output.append("#include <stdlib.h>\n");

        output.append("int main(int argc, char **argv) {\n");

        output.append("int64_t freq = -1;\n");
        output.append("void *labels[] = {\n");
        for (int pc = 0; pc < list.length; pc++) {
            output.append(String.format("  &&label_%d,\n", pc, pc));
        }
        output.append("};\n");

        for (int pc = 0; pc < list.length; pc++) {
            String str = list[pc];
            String[] op = str.split(" ");
            Instr instr = Instr.valueOf(op[0]);
            switch (instr) {
            case add:
            case jgz:
            case mod:
            case mul:
            case set:
                declareReg(op[1], output);
                declareReg(op[2], output);
                break;
            case rcv:
            case snd:
                declareReg(op[1], output);
                break;
            default:
                break;
            }
        }

        for (int pc = 0; pc < list.length; pc++) {

            String str = list[pc];
            String[] op = str.split(" ");
            Instr instr = Instr.valueOf(op[0]);

            output.append("\nlabel_" + pc + ":\n");
            switch (instr) {
            case add: {
                output.append(String.format("  %s += %s;%n", op[1], op[2]));
                break;
            }
            case mod: {
                output.append(String.format("  %s = %s %% %s;%n", op[1], op[1],
                        op[2]));
                break;
            }
            case mul: {
                output.append(String.format("  %s *= %s;%n", op[1], op[2]));
                break;
            }
            case set: {
                output.append(String.format("  %s = %s;%n", op[1], op[2]));
                break;
            }
            case jgz: {
                output.append(String.format(
                        "  if (%s > 0) goto *labels[%d + (%s)];%n", op[1], pc,
                        op[2]));
                break;
            }
            case rcv: {
                output.append(String.format( //
                        "  if (%s != 0) {\n" //
                                + "    printf(\"rcv = %%ld\\n\", freq);\n" //
                                + "    exit((int)freq);\n" //
                                + "  }\n", //
                        op[1]));
                break;
            }
            case snd: {
                output.append(String.format("  freq = %s;\n", op[1]));
                break;
            }
            default:
                break;
            }
        }

        output.append("  return 0;\n");
        output.append("}\n");
    }

    private int compileAndRun(String[] input)
            throws IOException, InterruptedException {
        StringBuilder builder = new StringBuilder();
        compile(input, builder);
        // System.out.println(builder.toString());
        Files.write(Paths.get("foo.c"), builder.toString().getBytes());

        ProcessBuilder pb = new ProcessBuilder("gcc", "-g", "-O2", "-Wall",
                "-Werror", "foo.c", "-o", "foo");
        pb.inheritIO();

        System.out.println("Compiling: " + pb.command());
        Process p = pb.start();
        assertEquals(0, p.waitFor());

        System.out.println("Running...");
        pb = new ProcessBuilder("./foo");
        pb.inheritIO();
        p = pb.start();
        p.waitFor();
        return p.exitValue();
    }

    @Test
    public void testShort() throws Exception {
        assertEquals(4, compileAndRun(input1));
    }

    @Test
    public void testFull() throws Exception {
        int freq = compileAndRun(getInputAsLines().toArray(n -> new String[n]));
        System.out.println("Day 18: frequency = " + freq);
        assertEquals(7071, freq);
    }
}
