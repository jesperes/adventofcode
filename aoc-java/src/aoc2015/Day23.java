package aoc2015;

import static org.junit.Assert.assertEquals;

import java.io.BufferedReader;
import java.io.FileReader;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.function.Function;

import org.junit.Test;

public class Day23 {
    enum Op {
        hlf {
            @Override
            public Instr<?, ?> parse(String[] line) {
                return new ArithmInstr(this, line[1], v -> v / 2);
            }
        },
        tpl {
            @Override
            public Instr<?, ?> parse(String[] line) {
                return new ArithmInstr(this, line[1], v -> v * 3);
            }
        },
        inc {
            @Override
            public Instr<?, ?> parse(String[] line) {
                return new ArithmInstr(this, line[1], v -> v + 1);
            }
        },
        jmp {
            @Override
            public Instr<?, ?> parse(String[] line) {
                return new JumpInstr(this, Integer.valueOf(line[1]));
            }
        },
        jio {
            @Override
            public Instr<?, ?> parse(String[] line) {
                return new CondJumpInstr(this, line[1],
                        Integer.valueOf(line[2]), v -> v == 1);
            }
        },
        jie {
            @Override
            public Instr<?, ?> parse(String[] line) {
                return new CondJumpInstr(this, line[1],
                        Integer.valueOf(line[2]), v -> v % 2 == 0);
            }
        };

        public Instr<?, ?> parse(String[] line) {
            return null;
        }
    }

    /**
     * hlf, tpl, and inc
     */
    static class ArithmInstr extends Instr<String, Void> {
        private Function<Integer, Integer> fun;

        public ArithmInstr(Op op, String arg1, Function<Integer, Integer> fun) {
            super(op, arg1, null);
            this.fun = fun;
        }

        @Override
        public int execute(Map<String, Integer> regs, int pc) {
            regs.put(arg1, fun.apply(regs.getOrDefault(arg1, 0)));
            return pc + 1;
        }
    }

    /**
     * jmp
     */
    static class JumpInstr extends Instr<Integer, Void> {
        public JumpInstr(Op op, Integer arg1) {
            super(op, arg1, null);
        }

        @Override
        public int execute(Map<String, Integer> regs, int pc) {
            return pc + arg1;
        }
    }

    /**
     * jio and jie
     */
    static class CondJumpInstr extends Instr<String, Integer> {
        private Function<Integer, Boolean> fun;

        public CondJumpInstr(Op op, String arg1, Integer arg2,
                Function<Integer, Boolean> fun) {
            super(op, arg1, arg2);
            this.fun = fun;
        }

        @Override
        public int execute(Map<String, Integer> regs, int pc) {
            if (fun.apply(regs.getOrDefault(arg1, 0)))
                return pc + arg2;
            else
                return pc + 1;
        }
    }

    static abstract class Instr<T1, T2> {
        final Op op;
        final T1 arg1;
        final T2 arg2;

        public Instr(Op op, T1 arg1, T2 arg2) {
            this.op = op;
            this.arg1 = arg1;
            this.arg2 = arg2;
        }

        public abstract int execute(Map<String, Integer> regs, int pc);
    }

    private int execute(List<Instr<?, ?>> instrs, int a) {
        int pc = 0;
        Map<String, Integer> regs = new HashMap<>();
        regs.put("a", a);
        while (pc < instrs.size()) {
            pc = instrs.get(pc).execute(regs, pc);
        }
        return regs.get("b");
    }

    @Test
    public void testDay23() throws Exception {
        List<Instr<?, ?>> instrs = new ArrayList<>();

        try (BufferedReader reader = new BufferedReader(
                new FileReader("inputs/2015/day23.txt"))) {
            String line;
            while ((line = reader.readLine()) != null) {
                String s[] = line.split("[ ,]+");
                instrs.add(Op.valueOf(s[0]).parse(s));
            }
        }

        assertEquals(255, execute(instrs, 0));
        assertEquals(334, execute(instrs, 1));
    }
}
