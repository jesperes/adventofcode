package aoc2017;

import java.io.File;
import java.io.IOException;
import java.util.Optional;
import java.util.stream.IntStream;

import aoc2017.Day15.Input;
import common2.AocBaseRunner;
import common2.AocPuzzleInfo;
import common2.AocResult;
import common2.IAocLongPuzzle;

public class Day15 implements IAocLongPuzzle<Input> {

	record Input(int a, int b) {
	}

	static class Generator {
		final long factor;
		final long initialValue;
		static private final long mod = 2147483647L;
		long value;

		public Generator(long factor, long value) {
			this.initialValue = value;
			this.factor = factor;
			this.value = value;
		}

		public void reset() {
			value = initialValue;
		}

		public long getValue() {
			return value;
		}

		public long generateNextValue() {
			value = (value * factor) % mod;
			return value;
		}
	}

	static class Generator_Part2 extends Generator {

		private long multiple;

		public Generator_Part2(long factor, long value, long multiple) {
			super(factor, value);
			this.multiple = multiple;
		}

		@Override
		public long generateNextValue() {
			while (true) {
				long nextValue = super.generateNextValue();
				if (nextValue % multiple == 0)
					return nextValue;
			}
		}
	}

	static class GeneratorA extends Generator {
		public GeneratorA(long value) {
			super(16807, value);
		}
	}

	static class GeneratorB extends Generator {
		public GeneratorB(long value) {
			super(48271, value);
		}
	}

	static class GeneratorA_Part2 extends Generator_Part2 {
		public GeneratorA_Part2(long value) {
			super(16807, value, 4);
		}
	}

	static class GeneratorB_Part2 extends Generator_Part2 {
		public GeneratorB_Part2(long value) {
			super(48271, value, 8);
		}
	}

	static boolean equalsLower16(long a, long b) {
		return (a & 0xffff) == (b & 0xffff);
	}

	@Override
	public AocPuzzleInfo getInfo() {
		return new AocPuzzleInfo(2017, 15, "Dueling Generators", false);
	}

	@Override
	public AocResult<Long, Long> getExpected() {
		return AocResult.of(592L, 320L);
	}

	@Override
	public Input parse(Optional<File> file) throws IOException {
		return new Input(277, 349);
	}

	@Override
	public Long part1(Input input) {
		Generator genA = new GeneratorA(input.a);
		Generator genB = new GeneratorB(input.b);

		// Consider the first 40 million pairs.
		return IntStream.range(0, 40_000_000)
				.filter(n -> equalsLower16(genA.generateNextValue(),
						genB.generateNextValue()))
				.count();
	}

	@Override
	public Long part2(Input input) {
		Generator genA = new GeneratorA_Part2(277);
		Generator genB = new GeneratorB_Part2(349);

		return IntStream.range(0, 5_000_000)
				.filter(n -> equalsLower16(genA.generateNextValue(),
						genB.generateNextValue()))
				.count();
	}

	public static void main(String[] args) {
		AocBaseRunner.run(new Day15());
	}
}
