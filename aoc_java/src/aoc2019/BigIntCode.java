package aoc2019;

import java.io.IOException;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Implementation of the Intcode machine using bigints. See e.g.
 * https://adventofcode.com/2019/day/9.
 * 
 * @author jesperes
 */
public class BigIntCode {

    /*
     * Opcodes
     */
    final int OP_ADD = 1;
    final int OP_MUL = 2;
    final int OP_INPUT = 3;
    final int OP_OUTPUT = 4;
    final int OP_JUMP_IF_TRUE = 5;
    final int OP_JUMP_IF_FALSE = 6;
    final int OP_LESS_THAN = 7;
    final int OP_EQUALS = 8;
    final int OP_ADJ_RELBASE = 9;
    final int OP_END = 99;

    /*
     * Operand modes
     */
    final int MODE_POS = 0;
    final int MODE_IMM = 1;
    final int MODE_REL = 2;

    /** The program */
    private Map<BigInteger, BigInteger> prog;

    /** Program counter */
    private BigInteger pc = BigInteger.ZERO;

    /** Base address for relative addressing */
    private BigInteger relbase = BigInteger.ZERO;

    private List<BigInteger> inputs;
    private List<BigInteger> outputs = new ArrayList<>();

    public static Map<BigInteger, BigInteger> parse(String prog) {
        Map<BigInteger, BigInteger> map = new HashMap<>();
        String[] strings = prog.split(",");
        for (int i = 0; i < strings.length; i++) {
            map.put(BigInteger.valueOf(i), new BigInteger(strings[i]));
        }
        return map;
    }

    public static List<BigInteger> listOf(int... ints) {
        List<BigInteger> list = new ArrayList<>();
        for (int x : ints) {
            list.add(BigInteger.valueOf(x));
        }
        return list;
    }

    public List<BigInteger> getOutputs() {
        return outputs;
    }

    public BigIntCode(Map<BigInteger, BigInteger> prog,
            List<BigInteger> inputs) {
        this.prog = prog;
        this.inputs = inputs;
        this.outputs = new ArrayList<>();
    }

    private BigInteger read(BigInteger op, int mode) {
        switch (mode) {
        case MODE_POS:
            return prog.getOrDefault(op, BigInteger.ZERO);
        case MODE_IMM:
            return op;
        case MODE_REL:
            return prog.getOrDefault(op.add(relbase), BigInteger.ZERO);
        default:
            assert false;
            return BigInteger.ZERO;
        }
    }

    private void write(BigInteger op, int mode, BigInteger value) {
        switch (mode) {
        case MODE_POS:
            prog.put(op, value);
            return;
        case MODE_REL:
            prog.put(op.add(relbase), value);
            return;
        default:
            assert false;
            return;
        }
    }

    public void execute() throws IOException {
        final BigInteger ZERO = BigInteger.ZERO;
        final BigInteger ONE = BigInteger.ONE;
        final BigInteger TWO = BigInteger.TWO;
        final BigInteger THREE = BigInteger.valueOf(3);
        final BigInteger FOUR = BigInteger.valueOf(4);

        while (true) {
            int op0 = read(pc, MODE_POS).intValue() % 100;
            int m1 = (read(pc, MODE_POS).intValue() / 100) % 10;
            int m2 = (read(pc, MODE_POS).intValue() / 1000) % 10;
            int m3 = (read(pc, MODE_POS).intValue() / 10000) % 10;

            BigInteger op1 = read(pc.add(ONE), MODE_POS);
            BigInteger op2 = read(pc.add(TWO), MODE_POS);
            BigInteger op3 = read(pc.add(THREE), MODE_POS);

            switch (op0) {
            case OP_ADD:
                write(op3, m3, read(op1, m1).add(read(op2, m2)));
                pc = pc.add(FOUR);
                break;
            case OP_MUL:
                write(op3, m3, read(op1, m1).multiply(read(op2, m2)));
                pc = pc.add(FOUR);
                break;
            case OP_JUMP_IF_TRUE:
                if (read(op1, m1).equals(ZERO))
                    pc = pc.add(THREE);
                else
                    pc = read(op2, m2);
                break;
            case OP_JUMP_IF_FALSE:
                if (read(op1, m1).equals(ZERO))
                    pc = read(op2, m2);
                else
                    pc = pc.add(THREE);
                break;
            case OP_LESS_THAN:
                if (read(op1, m1).compareTo(read(op2, m2)) < 0) {
                    write(op3, m3, ONE);
                    pc = pc.add(FOUR);
                } else {
                    write(op3, m3, ZERO);
                    pc = pc.add(FOUR);
                }
                break;
            case OP_EQUALS:
                if (read(op1, m1).equals(read(op2, m2))) {
                    write(op3, m3, ONE);
                    pc = pc.add(FOUR);
                } else {
                    write(op3, m3, ZERO);
                    pc = pc.add(FOUR);
                }
                break;
            case OP_ADJ_RELBASE:
                relbase = relbase.add(read(op1, m1));
                pc = pc.add(TWO);
                break;
            case OP_INPUT:
                write(op1, m1, input());
                pc = pc.add(TWO);
                break;
            case OP_OUTPUT:
                output(read(op1, m1));
                pc = pc.add(TWO);
                break;
            case OP_END:
                return;
            }
        }
    }

    protected void output(BigInteger output) {
        // System.out.println(output);
        outputs.add(output);
    }

    protected BigInteger input() {
        return inputs.remove(0);
    }
}
