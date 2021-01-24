package aoc2016;

import static org.junit.Assert.assertTrue;

import java.io.File;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;
import java.util.Optional;
import java.util.OptionalInt;
import java.util.stream.Collectors;

import aoc2016.Day25.Prog;
import common2.AocBaseRunner;
import common2.AocPuzzleInfo;
import common2.AocResult;
import common2.IAocIntPuzzle;
import common2.InputUtils;

public class Day25 implements IAocIntPuzzle<Prog> {

	enum Op {
		cpy, inc, jnz, dec, out;
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

	boolean execute(Prog prog, int[] regs, int alterCount) {
		int pc = 0;
		int lastOut = -1;
		var map = prog.map;

		while (true) {
			if (!map.containsKey(pc))
				return false;

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
			case out:
				int value = readReg(regs, x);
				if (lastOut == -1) {
					// first "out" value encountered
					lastOut = value;
				} else {
					assertTrue(value == 0 || value == 1);
					if (lastOut == value) {
						return false; // not alternating
					} else {
						lastOut = value;
						alterCount--;
						if (alterCount == 0)
							return true;
					}
				}
				pc++;
				break;
			default:
				throw new RuntimeException();
			}
		}
	}

	@Override
	public AocPuzzleInfo getInfo() {
		return new AocPuzzleInfo(2016, 25, "Clock Signal", true);
	}

	@Override
	public AocResult<Integer, Integer> getExpected() {
		return AocResult.of(180, null);
	}

	@Override
	public Integer part1(Prog input) {
		int[] regs = new int[4];
		for (int i = 0;; i++) {
			regs[0] = i;

			/*
			 * 8 is the minimal amount of out signals we need to observe to
			 * detect an "infinite" sequence.
			 */
			if (execute(input, regs, 8)) {
				return i;
			}
		}
	}

	@Override
	public Integer part2(Prog input) {
		return null;
	}

	public static void main(String[] args) {
		AocBaseRunner.run(new Day25());
	}
}
