package aoc2016;

import java.io.File;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;
import java.util.Optional;
import java.util.OptionalInt;
import java.util.stream.Collectors;

import aoc2016.Day12.Prog;
import common2.AocBaseRunner;
import common2.AocPuzzleInfo;
import common2.AocResult;
import common2.IAocIntPuzzle;
import common2.InputUtils;

public class Day12 implements IAocIntPuzzle<Prog> {

    enum Op {
        cpy, inc, jnz, dec
    }

    record Arg(OptionalInt value, OptionalInt regIndex) {
        static Arg of(String s) {
            char c = s.charAt(0);
            if (Character.isDigit(c) || c == '-') {
                // Literal integer
                return new Arg(OptionalInt.of(Integer.parseInt(s)),
                        OptionalInt.empty());
            } else {
                // Register
                return new Arg(OptionalInt.empty(),
                        OptionalInt.of(s.charAt(0) - 'a'));
            }
        }

        public String toString() {
            if (value.isPresent())
                return Integer.toString(value.getAsInt());
            else
                return String.format("%x", regIndex.getAsInt() + 'a');
        }
    }

    record Instr(Op op, Arg x, Arg y) {
    }

    record Prog(Map<Integer, Instr> map) {
    }

    @Override
    public AocPuzzleInfo getInfo() {
        return new AocPuzzleInfo(2016, 12, "Leonardo's Monorail", true);
    }

    @Override
    public AocResult<Integer, Integer> getExpected() {
        return AocResult.of(318003, 9227657);
    }

    @Override
    public Prog parse(Optional<File> file) throws IOException {
        var instrs = InputUtils.asStringList(file.get()).stream().map(line -> {
            String[] elems = line.split(" ");
            return new Instr(Op.valueOf(elems[0]), Arg.of(elems[1]),
                    (elems.length > 2) ? Arg.of(elems[2]) : null);
        }).collect(Collectors.toList());
        var map = new HashMap<Integer, Instr>();
        for (int pc = 0; pc < instrs.size(); pc++) {
            map.put(pc, instrs.get(pc));
        }
        return new Prog(map);
    }

    int readReg(int[] regs, Arg arg) {
        if (arg.value.isPresent())
            return arg.value.getAsInt();
        else
            return regs[arg.regIndex.getAsInt()];
    }

    void writeReg(int[] regs, Arg arg, int value) {
        regs[arg.regIndex.getAsInt()] = value;
    }

    int execute(Prog prog, int[] regs) {
        int pc = 0;
        var map = prog.map;

        while (true) {
            if (!map.containsKey(pc))
                return regs[0];

            Instr instr = map.get(pc);
            var y = instr.y;
            var x = instr.x;

            switch (instr.op) {
            case cpy:
                writeReg(regs, y, readReg(regs, x));
                pc++;
                break;
            case dec:
                writeReg(regs, x, readReg(regs, x) - 1);
                pc++;
                break;
            case inc:
                writeReg(regs, x, readReg(regs, x) + 1);
                pc++;
                break;
            case jnz:
                switch (readReg(regs, x)) {
                case 0:
                    pc++;
                    break;
                default:
                    pc += y.value.getAsInt();
                    break;
                }
                break;
            default:
                throw new RuntimeException();
            }
        }
    }

    @Override
    public Integer part1(Prog input) {
        return execute(input, new int[4]);
    }

    @Override
    public Integer part2(Prog input) {
        return execute(input, new int[] { 0, 0, 1, 0 });
    }

    public static void main(String[] args) {
        AocBaseRunner.run(new Day12());
    }
}
