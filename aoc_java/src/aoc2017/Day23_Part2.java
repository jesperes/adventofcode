package aoc2017;

import java.util.HashMap;
import java.util.Map;

import org.junit.Ignore;
import org.junit.Test;

import common.AocPuzzle;

public class Day23_Part2 extends AocPuzzle {
    public Day23_Part2() {
        super(2017, 23);
    }

    enum Op {
        set, sub, mul, jnz
    }

    static class Interpreter {
        private Map<String, Long> regs = new HashMap<>();
        private Map<Op, Long> stats = new HashMap<>();

        public Interpreter() {
            // Enable debug mode
            regs.put("a", 1L);
        }

        public long valueOf(String s) {
            try {
                return Integer.valueOf(s);
            } catch (NumberFormatException e) {
                return regs.getOrDefault(s, 0L);
            }
        }

        public Map<String, Long> getRegs() {
            return regs;
        }

        public Map<Op, Long> getStats() {
            return stats;
        }

        public void interpret(String[] program) {
            int pc = 0;

            while (pc >= 0 && pc < program.length) {
                String instr = program[pc];
                String[] ops = instr.split(" ");
                Op opcode = Op.valueOf(ops[0].trim());

                stats.put(opcode, stats.getOrDefault(opcode, 0L) + 1);

//                if (opcode == Op.jnz) {
//                    System.out.format("[%d] jump instruction '%s' (regs: %s)%n",
//                            pc, instr, regs);
//                }

                switch (opcode) {
                case set: {
                    regs.put(ops[1], valueOf(ops[2]));
                    pc++;
                    break;
                }
                case sub: {
                    regs.put(ops[1], valueOf(ops[1]) - valueOf(ops[2]));
                    pc++;
                    break;
                }
                case mul: {
                    regs.put(ops[1], valueOf(ops[1]) * valueOf(ops[2]));
                    pc++;
                    break;
                }
                case jnz: {
                    if (valueOf(ops[1]) != 0) {
                        pc += valueOf(ops[2]);
                    } else {
                        pc++;
                    }
                    break;
                }
                default:
                    throw new AssertionError();
                }
            }
        }
    }

    @Test
    @Ignore("does not work")
    public void test1() throws Exception {

        String[] program = getInputAsStream().filter(s -> !s.matches("^#"))
                .toArray(n -> new String[n]);

        Interpreter interpreter = new Interpreter();
        interpreter.interpret(program);

        System.out.println("Instruction statistics: " + interpreter.getStats());
        System.out.println("Register values: " + interpreter.getRegs());
    }
}
