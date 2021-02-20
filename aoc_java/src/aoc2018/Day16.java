package aoc2018;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;

import com.google.common.collect.Multimap;
import com.google.common.collect.MultimapBuilder;
import com.google.common.collect.Multimaps;

import aoc2018.Day16.Input;
import common2.AocBaseRunner;
import common2.AocPuzzleInfo;
import common2.AocResult;
import common2.IAocIntPuzzle;
import common2.InputUtils;

public class Day16 implements IAocIntPuzzle<Input> {
    record Regs(int[] array) {
    }

    record Instr(int opnr, int a, int b, int c) {
    }

    record Sample(Regs before, Instr instr, Regs after) {
    }

    record Input(List<Sample> samples, List<Instr> instrs) {
    }

    enum Op {
        addr, addi, mulr, muli, banr, bani, borr, bori, setr, seti, gtir, gtri,
        gtrr, eqir, eqri, eqrr
    }

    @Override
    public AocPuzzleInfo getInfo() {
        return new AocPuzzleInfo(2018, 16, "Chronal Classification", true);
    }

    @Override
    public AocResult<Integer, Integer> getExpected() {
        return AocResult.of(521, 594);
    }

    int toi(String s) {
        return Integer.parseInt(s.trim());
    }

    @Override
    public Input parse(Optional<File> file) throws IOException {

        List<Sample> samples = new ArrayList<>();
        List<Instr> instrs = new ArrayList<>();

        String[] parts = InputUtils.asString(file.get())
                .replaceAll("\r\n", "\n").split("\n\n\n");

        for (String line : parts[0].split("\n\n")) {
            String[] lines = line.split("\n");

            String[] before = lines[0].split("[: \\[\\],]+");
            String[] ops = lines[1].split(" ");
            String[] after = lines[2].split("[: \\[\\],]+");
            samples.add(new Sample(
                    new Regs(new int[] { toi(before[1]), toi(before[2]),
                            toi(before[3]), toi(before[4]) }),
                    new Instr(toi(ops[0]), toi(ops[1]), toi(ops[2]),
                            toi(ops[3])),
                    new Regs(new int[] { toi(after[1]), toi(after[2]),
                            toi(after[3]), toi(after[4]) })));
        }

        if (parts.length > 1) {
            for (String line : parts[1].split("\n")) {
                if (line.equals(""))
                    continue;
                String[] elems = line.split(" ");
                instrs.add(new Instr(toi(elems[0]), toi(elems[1]),
                        toi(elems[2]), toi(elems[3])));
            }
        }
        return new Input(samples, instrs);
    }

    @Override
    public Integer part1(Input input) {
        int count = 0;

        for (Sample sample : input.samples) {
            int n = 0;
            for (Op op : Op.values()) {
                Regs regs = new Regs(new int[4]);
                System.arraycopy(sample.before.array, 0, regs.array, 0, 4);
                execute(op, sample.instr, regs);
                if (Arrays.equals(regs.array, sample.after.array)) {
                    n++;
                }
            }

            if (n >= 3)
                count++;
        }

        return count;
    }

    @Override
    public Integer part2(Input input) {
        /*
         * Construct a multimap of opnr -> possible opcodes for that opnr.
         */
        Multimap<Integer, Op> map = MultimapBuilder.hashKeys().hashSetValues()
                .build();
        for (Sample sample : input.samples) {
            for (Op op : Op.values()) {
                Regs regs = new Regs(new int[4]);
                System.arraycopy(sample.before.array, 0, regs.array, 0, 4);
                execute(op, sample.instr, regs);
                if (Arrays.equals(sample.after.array, regs.array)) {
                    map.put(sample.instr.opnr, op);
                }
            }
        }

        /*
         * Compute the opnr -> opcode mapping by simply picking an opnr which
         * only maps to a single opcode, then filtering out that opnr -> opcode
         * mapping, and repeating until all opcodes are mapped.
         * 
         * Weirdly enough, this mapping is not the same as the one I got for my
         * Erlang solution, despite them using the same input.
         */
        Map<Integer, Op> opMap = new HashMap<>();
        while (opMap.size() < 16) {
            var e = map.asMap().entrySet().stream()
                    .filter(e0 -> e0.getValue().size() == 1).findFirst().get();
            var op = e.getValue().iterator().next();
            var opnr = e.getKey();
            opMap.put(opnr, op);
            map = Multimaps.filterEntries(map,
                    e0 -> (e0.getKey() != opnr) && (!e0.getValue().equals(op)));
        }

        /*
         * Run the program
         */
        Regs regs = new Regs(new int[4]);
        for (Instr instr : input.instrs) {
            execute(opMap.get(instr.opnr), instr, regs);
        }

        return regs.array[0];
    }

    /**
     * Execute instruction i under the assumption that i.opnr is op is the given
     * op.
     */
    void execute(Op op, Instr i, Regs r) {
        int[] regs = r.array;
        int a = i.a;
        int b = i.b;
        int c = i.c;
        // @formatter:off
        switch (op) {
        case addi: regs[c] = regs[a] + b;                  break;
        case addr: regs[c] = regs[a] + regs[b];            break;
        case bani: regs[c] = regs[a] & b;                  break;
        case banr: regs[c] = regs[a] & regs[b];            break;
        case bori: regs[c] = regs[a] | b;                  break;
        case borr: regs[c] = regs[a] | regs[b];            break;
        case eqir: regs[c] = (a == regs[b]) ? 1 : 0;       break;
        case eqri: regs[c] = (regs[a] == b) ? 1 : 0;       break;
        case eqrr: regs[c] = (regs[a] == regs[b]) ? 1 : 0; break;
        case gtir: regs[c] = (a > regs[b]) ? 1 : 0;        break;
        case gtri: regs[c] = (regs[a] > b) ? 1 : 0;        break;
        case gtrr: regs[c] = (regs[a] > regs[b]) ? 1 : 0;  break;
        case muli: regs[c] = regs[a] * b;                  break;
        case mulr: regs[c] = regs[a] * regs[b];            break;
        case seti: regs[c] = a;                            break; 
        case setr: regs[c] = regs[a];                      break;
        }
        // @formatter:on
    }

    public static void main(String[] args) {
        AocBaseRunner.run(new Day16());
    }
}
