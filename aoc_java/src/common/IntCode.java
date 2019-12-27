package common;

import static org.junit.Assert.fail;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Implementation of the Intcode machine using 64-bit ints. See e.g.
 * https://adventofcode.com/2019/day/9.
 * 
 * @author jesperes
 */
public class IntCode {

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

    /**
     * The program. We represent it as a map since the intcode spec says that
     * memory is "infinite", and any reads from uninitialized memory should
     * return 0.
     */
    private Map<Long, Long> prog;

    /** Program counter */
    private long pc = 0L;

    /** Base address for relative addressing */
    private long relbase = 0L;

    private List<Long> inputs = new ArrayList<>();
    private List<Long> outputs = new ArrayList<>();

    public static Map<Long, Long> parse(String prog) {
        Map<Long, Long> map = new HashMap<>();
        String[] strings = prog.split(",");
        for (int i = 0; i < strings.length; i++) {
            map.put((long) i, Long.valueOf(strings[i]));
        }
        return map;
    }

    public List<Long> getOutputs() {
        return outputs;
    }

    public IntCode(Map<Long, Long> prog, long... inputs) {
        this.prog = prog;
        for (long i : inputs) {
            this.inputs.add(i);
        }
        this.outputs = new ArrayList<>();
    }

    /**
     * Execute the given intcode program, supplying a list of inputs. Returns a
     * list containing the output values.
     * 
     * @param prog
     * @param inputs
     * @return
     * @return
     */
    public static List<Long> execute(String prog, long... inputs) {
        IntCode intcode = new IntCode(parse(prog), inputs);
        intcode.execute();
        return intcode.getOutputs();
    }

    private Long read(long op, int mode) {
        switch (mode) {
        case MODE_POS:
            return prog.getOrDefault(op, 0L);
        case MODE_IMM:
            return op;
        case MODE_REL:
            return prog.getOrDefault(op + relbase, 0L);
        default:
            fail();
            return 0L;
        }
    }

    private void write(Long op, int mode, Long value) {
        switch (mode) {
        case MODE_POS:
            prog.put(op, value);
            return;
        case MODE_REL:
            prog.put(op + relbase, value);
            return;
        default:
            assert false;
            return;
        }
    }

    public void execute() {

        while (true) {
            int op0 = read(pc, MODE_POS).intValue() % 100;
            int m1 = (read(pc, MODE_POS).intValue() / 100) % 10;
            int m2 = (read(pc, MODE_POS).intValue() / 1000) % 10;
            int m3 = (read(pc, MODE_POS).intValue() / 10000) % 10;

            Long op1 = read(pc + 1, MODE_POS);
            Long op2 = read(pc + 2, MODE_POS);
            Long op3 = read(pc + 3, MODE_POS);

            switch (op0) {
            case OP_ADD:
                write(op3, m3, read(op1, m1) + read(op2, m2));
                pc += 4;
                break;
            case OP_MUL:
                write(op3, m3, read(op1, m1) * read(op2, m2));
                pc += 4;
                break;
            case OP_JUMP_IF_TRUE:
                if (read(op1, m1).equals(0L))
                    pc += 3;
                else
                    pc = read(op2, m2);
                break;
            case OP_JUMP_IF_FALSE:
                if (read(op1, m1).equals(0L))
                    pc = read(op2, m2);
                else
                    pc += 3;
                break;
            case OP_LESS_THAN:
                if (read(op1, m1).compareTo(read(op2, m2)) < 0) {
                    write(op3, m3, 1L);
                    pc += 4;
                } else {
                    write(op3, m3, 0L);
                    pc += 4;
                }
                break;
            case OP_EQUALS:
                if (read(op1, m1).equals(read(op2, m2))) {
                    write(op3, m3, 1L);
                    pc += 4;
                } else {
                    write(op3, m3, 0L);
                    pc += 4;
                }
                break;
            case OP_ADJ_RELBASE:
                relbase += read(op1, m1);
                pc += 2;
                break;
            case OP_INPUT:
                write(op1, m1, input());
                pc += 2;
                break;
            case OP_OUTPUT:
                output(read(op1, m1));
                pc += 2;
                break;
            case OP_END:
                return;
            }
        }
    }

    protected void output(Long output) {
        outputs.add(output);
    }

    protected Long input() {
        return inputs.remove(0);
    }
}
