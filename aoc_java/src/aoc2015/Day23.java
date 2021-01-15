package aoc2015;

import java.io.File;
import java.io.IOException;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.function.Function;
import java.util.stream.Collectors;

import aoc2015.Day23.IInstr;
import common2.AocBaseRunner;
import common2.AocPuzzleInfo;
import common2.AocResult;
import common2.IAocIntPuzzle;
import common2.InputUtils;

public class Day23 implements IAocIntPuzzle<List<IInstr>> {
    enum Op {
        hlf {
            @Override
            public IInstr parse(String[] line) {
                return new ArithmInstr(this, line[1], v -> v / 2);
            }
        },
        tpl {
            @Override
            public IInstr parse(String[] line) {
                return new ArithmInstr(this, line[1], v -> v * 3);
            }
        },
        inc {
            @Override
            public IInstr parse(String[] line) {
                return new ArithmInstr(this, line[1], v -> v + 1);
            }
        },
        jmp {
            @Override
            public IInstr parse(String[] line) {
                return new JumpInstr(this, Integer.valueOf(line[1]));
            }
        },
        jio {
            @Override
            public IInstr parse(String[] line) {
                return new CondJumpInstr(this, line[1],
                        Integer.valueOf(line[2]), v -> v == 1);
            }
        },
        jie {
            @Override
            public IInstr parse(String[] line) {
                return new CondJumpInstr(this, line[1],
                        Integer.valueOf(line[2]), v -> v % 2 == 0);
            }
        };

        abstract public IInstr parse(String[] line);
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

    static abstract interface IInstr {
        abstract int execute(Map<String, Integer> regs, int pc);
    }

    static abstract class Instr<T1, T2> implements IInstr {
        final Op op;
        final T1 arg1;
        final T2 arg2;

        public Instr(Op op, T1 arg1, T2 arg2) {
            this.op = op;
            this.arg1 = arg1;
            this.arg2 = arg2;
        }
    }

    private int execute(List<IInstr> instrs, int a) {
        int pc = 0;
        Map<String, Integer> regs = new HashMap<>();
        regs.put("a", a);
        while (pc < instrs.size()) {
            pc = instrs.get(pc).execute(regs, pc);
        }
        return regs.get("b");
    }

    @Override
    public AocPuzzleInfo getInfo() {
        return new AocPuzzleInfo(2015, 23, "Opening the Turing Lock", true);
    }

    @Override
    public AocResult<Integer, Integer> getExpected() {
        return AocResult.of(255, 334);
    }

    @Override
    public List<IInstr> parse(Optional<File> file) throws IOException {
        return InputUtils.asStringList(file.get()).stream().map(line -> {
            String s[] = line.split("[ ,]+");
            return Op.valueOf(s[0]).parse(s);
        }).collect(Collectors.toUnmodifiableList());
    }

    @Override
    public Integer part1(List<IInstr> input) {
        return execute(input, 0);
    }

    @Override
    public Integer part2(List<IInstr> input) {
        return execute(input, 1);
    }

    public static void main(String[] args) {
        AocBaseRunner.run(new Day23());
    }
}
