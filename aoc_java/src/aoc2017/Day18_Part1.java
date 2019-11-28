package aoc2017;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;

import java.util.HashMap;
import java.util.Map;

import org.junit.Test;

import common.AocPuzzle;

public class Day18_Part1 extends AocPuzzle {
    public Day18_Part1() {
        super(2017, 18);
    }

    enum Instr {
        set, add, mul, mod, snd, rcv, jgz
    }

    // @formatter:off
	String[] input1 = {
		"set a 1",
		"add a 2",
		"mul a a",
		"mod a 5",
		"snd a",
		"set a 0",
		"rcv a",
		"jgz a -1",
		"set a 1",
		"jgz a -2"
	};
	// @formatter:on

    Map<Character, Long> registers = new HashMap<>();

    private long readValue(String str) {
        char c = str.charAt(0);
        if (c >= 'a' && c <= 'z')
            return registers.getOrDefault(str.charAt(0), 0L);
        else
            return Long.valueOf(str);
    }

    private char getReg(String str) {
        return str.charAt(0);
    }

    private long interpret(String[] list) {

        long freq = -1;

        for (int pc = 0; pc < list.length; pc++) {

            String str = list[pc];
            String[] words = str.split(" ");

            Instr instr = Instr.valueOf(words[0]);

            switch (instr) {
            case add: {
                char reg = getReg(words[1]);
                long value = readValue(words[2]);
                long oldvalue = registers.getOrDefault(reg, 0L);
                registers.put(reg, oldvalue + value);
                break;
            }
            case jgz: {
                long value = readValue(words[1]);
                long offset = readValue(words[2]);
                if (value > 0) {
                    pc += offset;
                    if (pc < 0 || pc >= list.length) {
                        fail("PC out of bounds");
                    }

                    pc--; // compensate for "pc++" in loop
                }
                break;
            }
            case mod: {
                char reg = getReg(words[1]);
                long value = readValue(words[2]);
                long oldvalue = registers.getOrDefault(reg, 0L);
                registers.put(reg, oldvalue % value);
                break;
            }
            case mul: {
                char reg = getReg(words[1]);
                long value = readValue(words[2]);
                long oldvalue = registers.getOrDefault(reg, 0L);
                registers.put(reg, oldvalue * value);
                break;
            }
            case rcv: {
                long value = readValue(words[1]);
                if (value != 0L) {
                    return freq;
                }
                break;
            }
            case set: {
                char reg = getReg(words[1]);
                long value = readValue(words[2]);
                registers.put(reg, value);
                break;
            }
            case snd: {
                freq = readValue(words[1]);
                break;
            }
            default:
                break;
            }

        }

        return -1;
    }

    @Test
    public void testShort() throws Exception {
        assertEquals(4, interpret(input1));
    }

    @Test
    public void testFull() throws Exception {
        long freq = interpret(getInputAsLines().toArray(n -> new String[n]));
        assertEquals(7071L, freq);
    }
}
