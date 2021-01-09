package aoc2020;

import static org.junit.Assert.assertEquals;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.util.HashSet;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

import aoc2020.Day08.Instr;
import common2.AocPuzzleInfo;
import common2.AocResult;
import common2.IAocPuzzle;

public class Day08 implements IAocPuzzle<List<Instr>, Integer, Integer> {
    enum Op {
        acc, jmp, nop
    }

    record Instr(Op op, int offset) {
    }

    record Result(int acc, boolean looped) {

    }

    @Override
    public AocPuzzleInfo getInfo() {
        return new AocPuzzleInfo(2020, 8, "Handheld Halting", true);
    }

    @Override
    public AocResult<Integer, Integer> getExpected() {
        return AocResult.of(1384, 761);
    }

    @Override
    public List<Instr> parse(Optional<File> file) {
        try {
            return Files.lines(file.get().toPath()).map(line -> {
                String[] elems = line.split(" ");
                return new Instr(Op.valueOf(elems[0]),
                        Integer.parseInt(elems[1]));
            }).collect(Collectors.toList());
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    @Override
    public Integer part1(List<Instr> input) {
        Result result = execute(input, -1);
        assertEquals(true, result.looped());
        return result.acc;
    }

    @Override
    public Integer part2(List<Instr> input) {
        for (int pc = 0; pc < input.size(); pc++) {
            Result result = execute(input, pc);
            if (!result.looped()) {
                return result.acc();
            }
        }
        throw new RuntimeException();
    }

    private Op flip(Op op) {
        if (op == Op.jmp)
            return Op.nop;
        else if (op == Op.nop)
            return Op.jmp;
        else
            return op;
    }

    private Result execute(List<Instr> input, int flip) {
        int pc = 0;
        int acc = 0;
        Set<Integer> executedInstrs = new HashSet<>();
        while (true) {
            if (executedInstrs.contains(pc)) {
                return new Result(acc, true);
            } else {
                executedInstrs.add(pc);
            }

            if (pc < 0 || pc >= input.size()) {
                return new Result(acc, false);
            }

            Instr instr = input.get(pc);
            Op op = (flip == pc) ? flip(instr.op) : instr.op;

            switch (op) {
            case acc:
                acc += instr.offset;
                pc++;
                continue;
            case jmp:
                pc += instr.offset;
                continue;
            case nop:
                pc++;
                continue;
            }

            throw new RuntimeException();
        }
    }
}
