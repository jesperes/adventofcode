package aoc2017;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;

import java.util.HashMap;
import java.util.LinkedList;
import java.util.Map;
import java.util.Queue;

import org.junit.Test;

import common.AocPuzzle;

public class Day18_Part2 extends AocPuzzle {

    public Day18_Part2() {
        super(2017, 18);
    }

    enum Instr {
        set, add, mul, mod, snd, rcv, jgz
    }

    // @formatter:off
	String[] input1 = {
        "snd 1",
        "snd 2",
        "snd p",
        "rcv a",
        "rcv b",
        "rcv c",
        "rcv d",
	};
	// @formatter:on

    Program[] programs;

    class Program {
        Map<Character, Long> registers = new HashMap<>();
        int pc = 0;
        int sendCount = 0;
        long progId;

        Queue<Long> msgQueue = new LinkedList<>();
        String[] code;

        public Program(String[] code, long progNum) {
            this.progId = progNum;
            registers.put('p', progNum);
            this.code = code;
        }

        long readValue(String str) {
            char c = str.charAt(0);
            if (c >= 'a' && c <= 'z')
                return registers.getOrDefault(str.charAt(0), 0L);
            else
                return Long.valueOf(str);
        }

        /**
         * Execute this program as far as possible, i.e. until the next rcv
         * instruction which blocks on an empty message queue.
         * 
         * @param str
         * @return The number of instructions executed.
         */
        int execute() {
            int instructions = 0;

            while (true) {
                String[] words = code[pc].split(" ");
                Instr instr = Instr.valueOf(words[0]);

                // System.out.format("[PROG %d] PC(%d) %s%n", progId, pc,
                // code[pc]);
                switch (instr) {
                case add: {
                    char reg = getReg(words[1]);
                    long value = readValue(words[2]);
                    long oldvalue = registers.getOrDefault(reg, 0L);
                    registers.put(reg, oldvalue + value);
                    pc++;
                    instructions++;
                    continue;
                }
                case jgz: {
                    long value = readValue(words[1]);
                    long offset = readValue(words[2]);
                    if (value > 0) {
                        pc += offset;
                    } else {
                        pc++;
                    }
                    instructions++;
                    continue;
                }
                case mod: {
                    char reg = getReg(words[1]);
                    long value = readValue(words[2]);
                    long oldvalue = registers.getOrDefault(reg, 0L);
                    registers.put(reg, oldvalue % value);
                    pc++;
                    instructions++;
                    continue;
                }
                case mul: {
                    char reg = getReg(words[1]);
                    long value = readValue(words[2]);
                    long oldvalue = registers.getOrDefault(reg, 0L);
                    registers.put(reg, oldvalue * value);
                    pc++;
                    instructions++;
                    continue;
                }
                case set: {
                    char reg = getReg(words[1]);
                    long value = readValue(words[2]);
                    registers.put(reg, value);
                    pc++;
                    instructions++;
                    continue;
                }
                case rcv: {
                    char reg = getReg(words[1]);

                    if (msgQueue.isEmpty()) {
                        return instructions;
                    } else {
                        pc++;
                        long value = msgQueue.poll();
                        registers.put(reg, value);
                        continue;
                    }
                }
                case snd: {
                    long value = readValue(words[1]);
                    Program prog = programs[(int) ((progId + 1) % 2)];
                    prog.msgQueue.add(value);
                    pc++;
                    instructions++;
                    sendCount++;
                    // System.out.format("Program %d send count is now %s%n",
                    // progId, sendCount);
                    continue;
                }
                default:
                    fail("unknown instruction");
                }
            }
        }
    }

    private static char getReg(String str) {
        return str.charAt(0);
    }

    private void interpret(Program[] programs) {

        Program prog0 = programs[0];
        Program prog1 = programs[1];

        while (true) {
            int steps0 = prog0.execute();
            int steps1 = prog1.execute();

            if (steps0 == 0 && steps1 == 0) {
                return;
            }
        }
    }

    @Test
    public void testShort() throws Exception {
        programs = new Program[2];
        programs[0] = new Program(input1, 0);
        programs[1] = new Program(input1, 1);
        interpret(programs);
        assertEquals(3, programs[1].sendCount);
    }

    @Test
    public void testFull() throws Exception {

        String[] input = getInputAsLines().toArray(n -> new String[n]);

        programs = new Program[2];
        programs[0] = new Program(input, 0);
        programs[1] = new Program(input, 1);

        interpret(programs);

        int prog1SendCount = programs[1].sendCount;

        assertEquals(8001, prog1SendCount);
    }
}
