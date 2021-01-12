package aoc2015;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.nio.file.Files;
import java.util.HashMap;
import java.util.Map;
import java.util.Optional;
import java.util.stream.Collectors;

import aoc2015.Day07.Instr;
import common2.AocBaseRunner;
import common2.AocPuzzleInfo;
import common2.AocResult;
import common2.IAocIntPuzzle;

public class Day07 implements IAocIntPuzzle<Map<String, Instr>> {

    enum Op {
        AND, NOT, OR, RSHIFT, LSHIFT, ASSIGN
    }

    record Operand(String s, int n) {
        Operand(String str) {
            this(str, toInt(str));
        }

        static int toInt(String str) {
            if (str.length() > 0 && Character.isDigit(str.charAt(0)))
                return Integer.parseInt(str);
            else
                return -1;
        }

        static Operand of(String str) {
            return new Operand(str);
        }

        static Operand empty() {
            return new Operand("");
        }

        boolean isInt() {
            return n >= 0;
        }
    }

    record Instr(Op op, Operand a, Operand b, Operand c) {
        static Instr fromStr(String line) {
            String s[] = line.split(" ");

            if (s[0].equals("NOT")) {
                return new Instr(Op.NOT, Operand.of(s[1]), Operand.of(s[3]),
                        Operand.empty());
            } else if (s[1].equals("->")) {
                return new Instr(Op.ASSIGN, Operand.of(s[0]), Operand.of(s[2]),
                        Operand.empty());
            } else {
                return new Instr(Op.valueOf(s[1]), Operand.of(s[0]),
                        Operand.of(s[2]), Operand.of(s[4]));
            }
        }

        private String dest() {
            switch (op) {
            case NOT:
                return b.s();
            case ASSIGN:
                return b.s();
            default:
                return c.s();
            }
        }

    }

    @Override
    public AocPuzzleInfo getInfo() {
        return new AocPuzzleInfo(2015, 7, "Some Assembly Required", true);
    }

    @Override
    public AocResult<Integer, Integer> getExpected() {
        return AocResult.of(956, 40149);
    }

    @Override
    public Map<String, Instr> parse(Optional<File> file) throws IOException {
        return Files.lines(file.get().toPath()).map(Instr::fromStr)
                .collect(Collectors.toUnmodifiableMap(instr -> instr.dest(),
                        instr -> instr));
    }

    private int findSignal(Map<String, Instr> instrs,
            Map<Operand, Integer> wires, Operand wire) {
        if (wires.containsKey(wire))
            return wires.get(wire);

        if (wire.isInt())
            return wire.n();

        Instr instr = instrs.get(wire.s);
        int c;
        switch (instr.op) {
        case AND: {
            var a = findSignal(instrs, wires, instr.a);
            var b = findSignal(instrs, wires, instr.b);
            c = a & b;
            break;
        }
        case OR: {
            var a = findSignal(instrs, wires, instr.a);
            var b = findSignal(instrs, wires, instr.b);
            c = a | b;
            break;
        }
        case LSHIFT: {
            var a = findSignal(instrs, wires, instr.a);
            var b = instr.b.n();
            c = a << b;
            break;
        }
        case RSHIFT: {
            var a = findSignal(instrs, wires, instr.a);
            var b = instr.b.n();
            c = a >> b;
            break;
        }
        case ASSIGN: {
            c = findSignal(instrs, wires, instr.a);
            break;
        }
        case NOT: {
            c = ~(findSignal(instrs, wires, instr.a));
            break;
        }
        default:
            throw new RuntimeException();
        }

        wires.put(wire, c);
        return c;
    }

    @Override
    public Integer part1(Map<String, Instr> instrs) {
        return findSignal(instrs, new HashMap<>(), Operand.of("a"));
    }

    @Override
    public Integer part2(Map<String, Instr> instrs) {
        Map<Operand, Integer> wires = new HashMap<>();
        wires.put(Operand.of("b"), getExpected().p1().get());
        return findSignal(instrs, wires, Operand.of("a"));
    }

    public static void main(String[] args) throws FileNotFoundException {
        AocBaseRunner.run(new Day07());
    }
}
