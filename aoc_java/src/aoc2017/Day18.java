package aoc2017;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.io.File;
import java.io.IOException;
import java.util.List;
import java.util.Optional;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingDeque;
import java.util.stream.Collectors;

import org.junit.Test;

import aoc2017.Day18.Instr;
import common2.AocBaseRunner;
import common2.AocPuzzleInfo;
import common2.AocResult;
import common2.IAocLongPuzzle;
import common2.InputUtils;

public class Day18 implements IAocLongPuzzle<List<Instr>> {
	enum Op {
		set, add, mul, mod, snd, rcv, jgz, eop;
	}

	record Arg(long value, boolean isReg) {
		static Arg fromStr(String str) {
			if (str == null)
				return null;
			if (Character.isLetter(str.charAt(0))) {
				return new Arg(str.charAt(0) - 'a', true);
			} else {
				return new Arg(Integer.parseInt(str), false);
			}
		}
	}

	record Instr(Op op, Arg x, Arg y) {
	}

	abstract class Program {
		long[] regs = new long[26];
		int pc = 0;
		final List<Instr> prog;
		boolean eop = false;
		int instructions;
		int id;

		public Program(List<Instr> prog, int id) {
			this.prog = prog;
			this.regs['p' - 'a'] = id;
			this.id = id;
		}

		long read(Arg a) {
			return a.isReg ? regs[(int) a.value] : a.value;
		}

		void write(Arg a, long val) {
			assertTrue(a.isReg);
			regs[(int) a.value] = val;
		}

		public void run() {
			instructions = 0;

			while (!eop) {
				if (pc < 0 || pc >= prog.size())
					return;

				Instr instr = prog.get(pc);
				Arg x = instr.x;
				Arg y = instr.y;

				switch (instr.op) {
				case add: {
					write(x, read(x) + read(y));
					pc++;
					instructions++;
					break;
				}
				case mod: {
					write(x, read(x) % read(y));
					pc++;
					instructions++;
					break;
				}
				case mul: {
					write(x, read(x) * read(y));
					pc++;
					instructions++;
					break;
				}
				case set: {
					write(x, read(y));
					pc++;
					instructions++;
					break;
				}
				case jgz: {
					pc += (read(x) > 0) ? read(y) : 1;
					instructions++;
					break;
				}
				case rcv: {
					rcv(instr);
					if (eop)
						return;
					pc++;
					instructions++;
					break;
				}
				case snd: {
					snd(instr);
					pc++;
					instructions++;
					break;
				}
				default:
					throw new RuntimeException();
				}
			}
		}

		abstract void snd(Instr instr);

		abstract void rcv(Instr instr);
	}

	@Override
	public AocPuzzleInfo getInfo() {
		return new AocPuzzleInfo(2017, 18, "Duet", true);
	}

	@Override
	public AocResult<Long, Long> getExpected() {
		return AocResult.of(7071L, 8001L);
	}

	@Override
	public List<Instr> parse(Optional<File> file) throws IOException {
		return InputUtils.asStringList(file.get()).stream().map(line -> {
			String[] elems = line.split(" ");
			Op op = Op.valueOf(elems[0]);
			String a = elems[1];
			String b = (elems.length >= 3) ? elems[2] : null;
			return new Instr(op, Arg.fromStr(a), Arg.fromStr(b));
		}).collect(Collectors.toUnmodifiableList());
	}

	/*
	 * Part 1
	 */
	class ProgramPart1 extends Program {
		long freq = -1;

		public ProgramPart1(List<Instr> prog) {
			super(prog, 0);
		}

		@Override
		void snd(Instr instr) {
			freq = read(instr.x);
		}

		@Override
		void rcv(Instr instr) {
			if (read(instr.x) != 0) {
				eop = true;
			}
		}
	}

	@Override
	public Long part1(List<Instr> prog) {
		ProgramPart1 p1 = new ProgramPart1(prog);
		p1.run();
		return p1.freq;
	}

	/*
	 * Part 2
	 */
	class ProgramPart2 extends Program {
		BlockingQueue<Long> queue = new LinkedBlockingDeque<>();
		ProgramPart2 other;
		int count = 0;

		public ProgramPart2(List<Instr> prog, int id) {
			super(prog, id);
		}

		public void setOther(ProgramPart2 other) {
			this.other = other;
		}

		@Override
		void snd(Instr instr) {
			count++;
			other.queue.add(read(instr.x));
		}

		@Override
		void rcv(Instr instr) {
			Long val = queue.poll();
			if (val == null) {
				eop = true;
			} else {
				write(instr.x, val);
			}
		}
	}

	@Override
	public Long part2(List<Instr> input) {
		ProgramPart2 p0 = new ProgramPart2(input, 0);
		ProgramPart2 p1 = new ProgramPart2(input, 1);
		p1.setOther(p0);
		p0.setOther(p1);

		while (true) {
			p0.run();
			p1.run();

			if (p0.instructions == 0 && p1.instructions == 0) {
				return (long) p1.count;
			}

			p0.eop = false;
			p1.eop = false;
		}
	}

	@Test
	public void testName() throws Exception {
		String prog = """
				set a 1
				add a 2
				mul a a
				mod a 5
				snd a
				set a 0
				rcv a
				jgz a -1
				set a 1
				jgz a -2
				""";

		List<Instr> data = InputUtils.parseTestData(this, prog);
		long result = part1(data);
		assertEquals(4L, result);

	}

	public static void main(String[] args) {
		AocBaseRunner.run(new Day18());
	}
}
