package aoc2015;

import static org.junit.Assert.assertEquals;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.OptionalInt;

import org.junit.Test;

public class Day07 {

    enum Op {
        AND, NOT, OR, RSHIFT, LSHIFT, ASSIGN
    }

    class Instr {
        Op op;
        String a, b, c;
    }

    @FunctionalInterface
    interface BinaryOpFun {
        public int apply(int a, int b);
    }

    @FunctionalInterface
    interface UnaryOpFun {
        public int apply(int a);
    }

    private OptionalInt readWire(Map<String, Integer> wires, String wire) {
        try {
            return OptionalInt.of(Integer.valueOf(wire));
        } catch (NumberFormatException e) {
            if (wires.containsKey(wire)) {
                return OptionalInt.of(wires.get(wire));
            } else {
                return OptionalInt.empty();
            }
        }
    }

    private void doBinaryOp(Instr instr, Map<String, Integer> wires,
            BinaryOpFun fun) {
        if (wires.containsKey(instr.c))
            return;

        OptionalInt valueA = readWire(wires, instr.a);
        OptionalInt valueB = readWire(wires, instr.b);
        if (valueA.isPresent() && valueB.isPresent()) {
            wires.put(instr.c, fun.apply(valueA.getAsInt(), valueB.getAsInt()));
        }
    }

    private void doUnaryOp(Instr instr, Map<String, Integer> wires,
            UnaryOpFun fun) {
        if (wires.containsKey(instr.b))
            return;

        OptionalInt valueA = readWire(wires, instr.a);
        if (valueA.isPresent()) {
            wires.put(instr.b, fun.apply(valueA.getAsInt()));
        }
    }

    private void runPass(List<Instr> instrs, Map<String, Integer> wires) {
        for (Instr instr : instrs) {
            switch (instr.op) {
            case AND:
                doBinaryOp(instr, wires, (a, b) -> a & b);
                break;
            case OR:
                doBinaryOp(instr, wires, (a, b) -> a | b);
                break;
            case LSHIFT:
                doBinaryOp(instr, wires, (a, b) -> a << b);
                break;
            case RSHIFT:
                doBinaryOp(instr, wires, (a, b) -> a >> b);
                break;
            case NOT:
                doUnaryOp(instr, wires, (a) -> ~a);
                break;
            case ASSIGN:
                doUnaryOp(instr, wires, (a) -> a);
            }
        }
    }

    private int runUntilWire(List<Instr> instrs, Map<String, Integer> wires,
            String wire) {
        while (!wires.containsKey(wire)) {
            runPass(instrs, wires);
        }
        return wires.get(wire);
    }

    @Test
    public void testDay07() throws IOException {

        List<Instr> instrs = new ArrayList<>();

        try (BufferedReader reader = new BufferedReader(
                new FileReader("inputs/2015/day07.txt"))) {
            String line;
            while ((line = reader.readLine()) != null) {
                String s[] = line.split(" ");
                Instr instr = new Instr();

                if (s[0].equals("NOT")) {
                    instr.op = Op.NOT;
                    instr.a = s[1];
                    instr.b = s[3];
                    instr.c = null;
                } else if (s[1].equals("->")) {
                    instr.op = Op.ASSIGN;
                    instr.a = s[0];
                    instr.b = s[2];
                    instr.c = null;
                } else {
                    instr.op = Op.valueOf(s[1]);
                    instr.a = s[0];
                    instr.b = s[2];
                    instr.c = s[4];
                }

                instrs.add(instr);
            }
        }

        Map<String, Integer> wires = new HashMap<>();
        int p1 = runUntilWire(instrs, wires, "a");
        assertEquals(956, p1);

        wires.clear();
        wires.put("b", p1);
        int p2 = runUntilWire(instrs, wires, "a");
        assertEquals(40149, p2);
    }

}
