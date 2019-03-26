package aoc2015;

import static org.junit.Assert.assertEquals;

import java.io.BufferedReader;
import java.io.FileReader;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.junit.Test;

public class Day23 {
    enum Op {
        hlf {
            @Override
            public Instr<?, ?> parse(String[] line) {
                return new Instr<String, Void>(this, line[1], null) {
                    @Override
                    public int execute(Map<String, Integer> regs, int pc) {
                        regs.put(arg1, regs.getOrDefault(arg1, 0) / 2);
                        return pc + 1;
                    }
                };
            }
        },
        tpl {
            @Override
            public Instr<?, ?> parse(String[] line) {
                return new Instr<String, Void>(this, line[1], null) {
                    @Override
                    public int execute(Map<String, Integer> regs, int pc) {
                        regs.put(arg1, regs.getOrDefault(arg1, 0) * 3);
                        return pc + 1;
                    }
                };
            }
        },
        inc {
            @Override
            public Instr<?, ?> parse(String[] line) {
                return new Instr<String, Void>(this, line[1], null) {
                    @Override
                    public int execute(Map<String, Integer> regs, int pc) {
                        regs.put(arg1, regs.getOrDefault(arg1, 0) + 1);
                        return pc + 1;
                    }
                };
            }
        },
        jmp {
            @Override
            public Instr<?, ?> parse(String[] line) {
                return new Instr<Integer, Void>(this, Integer.valueOf(line[1]),
                        null) {

                    @Override
                    public int execute(Map<String, Integer> regs, int pc) {
                        return pc + arg1;
                    }
                };
            }

        },
        jio {
            @Override
            public Instr<?, ?> parse(String[] line) {
                return new Instr<String, Integer>(this, line[1],
                        Integer.valueOf(line[2])) {
                    @Override
                    public int execute(Map<String, Integer> regs, int pc) {
                        if (regs.getOrDefault(arg1, 0) == 1) {
                            return pc + arg2;
                        } else {
                            return pc + 1;
                        }
                    }
                };
            }

        },
        jie {
            @Override
            public Instr<?, ?> parse(String[] line) {
                return new Instr<String, Integer>(this, line[1],
                        Integer.valueOf(line[2])) {
                    @Override
                    public int execute(Map<String, Integer> regs, int pc) {
                        if (regs.getOrDefault(arg1, 0) % 2 == 0) {
                            return pc + arg2;
                        } else {
                            return pc + 1;
                        }
                    }
                };
            }
        };

        public Instr<?, ?> parse(String[] line) {
            return null;
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
