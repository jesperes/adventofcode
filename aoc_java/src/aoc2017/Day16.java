package aoc2017;

import java.io.File;
import java.io.IOException;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.stream.Collectors;

import aoc2017.Day16.Instruction;
import common2.AocBaseRunner;
import common2.AocPuzzleInfo;
import common2.AocResult;
import common2.IAocPuzzle;
import common2.InputUtils;

public class Day16 implements IAocPuzzle<List<Instruction>, String, String> {

	abstract class Instruction {
		abstract public void perform(char[] actor);
	}

	class Spin extends Instruction {
		final private int count;
		char tmp[];

		Spin(int n) {
			count = n;
		}

		@Override
		public String toString() {
			return "s" + String.valueOf(count);
		}

		@Override
		public void perform(char[] actors) {
			// Allocate temp array
			if (tmp == null)
				tmp = new char[actors.length];

			if (count == 0 || count == actors.length)
				return;

			if (count < 0 || count >= actors.length) {
				throw new AssertionError("invalid count: " + count);
			}

			// Move the last count actors to the temporary array
			for (int i = 0; i < count; i++) {
				tmp[i] = actors[actors.length - count + i];
			}

			// Move actors to the end.
			for (int i = actors.length - 1; i >= count; i--) {
				int src_idx = i - count;
				int dest_idx = i;
				actors[dest_idx] = actors[src_idx];
			}

			// Copy actors back from temp storage.
			for (int i = 0; i < count; i++) {
				actors[i] = tmp[i];
			}
		}
	}

	class Exchange extends Instruction {
		final private int indexa;
		final private int indexb;

		Exchange(int indexa, int indexb) {
			this.indexa = indexa;
			this.indexb = indexb;
		}

		@Override
		public void perform(char[] actors) {
			char tmp = actors[indexa];
			actors[indexa] = actors[indexb];
			actors[indexb] = tmp;
		}
	}

	class Partner extends Instruction {
		final private char proga;
		final private char progb;

		public Partner(char proga, char progb) {
			this.proga = proga;
			this.progb = progb;
		}

		@Override
		public String toString() {
			return String.format("p%c/%c", proga, progb);
		}

		@Override
		public void perform(char[] actors) {
			int indexa = -1;
			int indexb = -1;

			for (int i = 0; i < actors.length; i++) {
				if (actors[i] == proga)
					indexa = i;
				if (actors[i] == progb)
					indexb = i;

				// We've found both programs, do the swap
				if (indexa >= 0 && indexb >= 0) {
					char tmp = actors[indexa];
					actors[indexa] = actors[indexb];
					actors[indexb] = tmp;
					return;
				}
			}
		}
	}

	public void performDance(char[] actors, List<Instruction> instructions) {
		for (Instruction instr : instructions) {
			instr.perform(actors);
		}
	}

	@Override
	public AocPuzzleInfo getInfo() {
		return new AocPuzzleInfo(2017, 16, "Permutation Promenade", true);
	}

	@Override
	public AocResult<String, String> getExpected() {
		return AocResult.of("kbednhopmfcjilag", "fbmcgdnjakpioelh");
	}

	@Override
	public List<Instruction> parse(Optional<File> file) throws IOException {
		return Arrays.stream(InputUtils.asString(file.get()).trim().split(","))
				.map(instr -> {
					switch (instr.charAt(0)) {
					case 's': // Spin
						return new Spin(Integer.valueOf(instr.substring(1)));
					case 'x': // Exchange
					{
						String elems[] = instr.substring(1).split("/");
						return new Exchange(Integer.valueOf(elems[0]),
								Integer.valueOf(elems[1]));
					}
					case 'p': // Partner
						return new Partner(instr.charAt(1), instr.charAt(3));
					default:
						throw new RuntimeException();
					}
				}).collect(Collectors.toList());
	}

	@Override
	public String part1(List<Instruction> input) {
		char actors[] = "abcdefghijklmnop".toCharArray();
		performDance(actors, input);
		return new String(actors);
	}

	@Override
	public String part2(List<Instruction> input) {
		char actors[] = "abcdefghijklmnop".toCharArray();

		Map<String, String> cache = new HashMap<>();
		int repeat = 1_000_000_000;

		String state = new String(actors);

		for (int i = 0; i < repeat; i++) {
			String newState = cache.getOrDefault(state, null);

			if (newState == null) {
				actors = state.toCharArray();
				performDance(actors, input);
				cache.put(state, new String(actors));
			} else {
				state = newState;
			}
		}

		return state;
	}

	public static void main(String[] args) {
		AocBaseRunner.run(new Day16());
	}
}
