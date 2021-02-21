package aoc2018;

import java.io.File;
import java.io.IOException;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

import aoc2018.Day19.Instr;
import common2.AocBaseRunner;
import common2.AocPuzzleInfo;
import common2.AocResult;
import common2.IAocIntPuzzle;
import common2.InputUtils;

public class Day19 implements IAocIntPuzzle<List<Instr>> {

    static final int IP = 5;

    record Regs(int[] array) {
    }

    record Instr(Op op, int a, int b, int c) {
    }

    enum Op {
        addr(14), addi(4), mulr(15), muli(8), banr(13), bani(10), borr(11),
        bori(9), setr(5), seti(1), gtir(12), gtri(7), gtrr(6), eqir(0), eqri(2),
        eqrr(3);

        int opcode;

        Op(int opcode) {
            this.opcode = opcode;
        }
    }

    @Override
    public AocPuzzleInfo getInfo() {
        return new AocPuzzleInfo(2018, 19, "Go With The Flow", true);
    }

    @Override
    public AocResult<Integer, Integer> getExpected() {
        return AocResult.of(1500, 18869760);
    }

    @Override
    public List<Instr> parse(Optional<File> file) throws IOException {
        return InputUtils.asStringList(file.get()).stream().skip(1)
                .map(line -> {
                    String[] elems = line.split(" ");
                    Op op = Op.valueOf(elems[0]);
                    int a = Integer.parseInt(elems[1]);
                    int b = Integer.parseInt(elems[2]);
                    int c = Integer.parseInt(elems[3]);
                    return new Instr(op, a, b, c);
                }).collect(Collectors.toList());
    }

    @Override
    public Integer part1(List<Instr> input) {
        Regs r = new Regs(new int[6]);
        int[] regs = r.array;

        while (true) {
            int ip = regs[IP];
            if (ip < 0 || ip >= input.size())
                break;

            Instr instr = input.get(ip);
            Op op = instr.op;

            int a = instr.a;
            int b = instr.b;
            int c = instr.c;

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

            regs[IP]++;
        }

        return regs[0];
    }

    @Override
    public Integer part2(List<Instr> input) {
        /*
         * Part 2 (where R0 is set to 1) computes (ineffectively) the sum of the
         * factors of a "magic number" so we solve directly instead.
         */
        int magic = 10551398;
        int sum = 0;
        for (int i = 1; i <= magic; i++) {
            if (magic % i == 0) {
                sum += i;
            }
        }
        return sum;
    }

    public static void main(String[] args) {
        AocBaseRunner.run(new Day19());
    }
}
