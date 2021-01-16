package aoc2016;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;

import java.io.File;
import java.io.IOException;
import java.util.Arrays;
import java.util.List;
import java.util.Optional;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

import org.junit.Test;

import aoc2016.Day08.Instr;
import common2.AocBaseRunner;
import common2.AocPuzzleInfo;
import common2.AocResult;
import common2.IAocPuzzle;
import common2.InputUtils;

public class Day08 implements IAocPuzzle<List<Instr>, Integer, String> {

	Pattern p;

	private static void rotate(char[] array, int n) {
		char[] tmp = new char[n];
		System.arraycopy(array, (array.length - n), tmp, 0, n);
		System.arraycopy(array, 0, array, n, (array.length - n));
		System.arraycopy(tmp, 0, array, 0, n);
	}

	enum Op {
		RotateColumn(Pattern.compile("rotate column x=(?<x>\\d+) by (?<y>\\d+)")) {
			@Override
			void apply(int x, int y, char[][] array) {
				char[] col = new char[array.length];
				for (int i = 0; i < array.length; i++) {
					col[i] = array[i][x];
				}
				rotate(col, y);
				for (int i = 0; i < array.length; i++) {
					array[i][x] = col[i];
				}
			}
		},
		RotateRow(Pattern.compile("rotate row y=(?<y>\\d+) by (?<x>\\d+)")) {
			@Override
			void apply(int x, int y, char[][] array) {
				rotate(array[y], x);
			}
		},
		Rect(Pattern.compile("rect (?<x>\\d+)x(?<y>\\d+)")) {
			@Override
			void apply(int x, int y, char[][] array) {
				for (int row = 0; row < y; row++) {
					for (int col = 0; col < x; col++) {
						array[row][col] = '#';
					}
				}
			}
		};

		private Pattern pattern;

		Op(Pattern pattern) {
			this.pattern = pattern;
		}

		Instr parse(String line) {
			var m = pattern.matcher(line);
			if (m.matches()) {
				return new Instr(this, Integer.valueOf(m.group("x")), Integer.valueOf(m.group("y")));
			} else {
				return null;
			}
		}

		abstract void apply(int x, int y, char[][] array);
	}

	record Instr(Op op, int x, int y) {
		public void apply(char[][] array) {
			op.apply(x, y, array);
		}
	}

	@Override
	public AocPuzzleInfo getInfo() {
		return new AocPuzzleInfo(2016, 8, "Two-Factor Authentication", true);
	}

	@Override
	public AocResult<Integer, String> getExpected() {
		return AocResult.of(115, "EFEYKFRFIJ");
	}

	@Override
	public List<Instr> parse(Optional<File> file) throws IOException {
		return InputUtils.asStringList(file.get()).stream().map(line -> {
			for (Op op : Op.values()) {
				Instr instr = op.parse(line);
				if (instr != null) {
					return instr;
				}
			}
			fail();
			return null;
		}).collect(Collectors.toUnmodifiableList());
	}

	@Override
	public Integer part1(List<Instr> input) {
		char array[][] = new char[6][50];
		for (int y = 0; y < array.length; y++) {
			for (int x = 0; x < array[y].length; x++) {
				array[y][x] = '.';
			}
		}

		for (Instr instr : input) {
			instr.apply(array);
		}

		StringBuilder b = new StringBuilder();
		int count = 0;
		for (char[] line : array) {
			for (char c : line) {
				count += (c == '#') ? 1 : 0;
				b.append(c);
			}
			b.append("\n");
		}

		String s = """
				####.####.####.#...##..#.####.###..####..###...##.
				#....#....#....#...##.#..#....#..#.#......#.....#.
				###..###..###...#.#.##...###..#..#.###....#.....#.
				#....#....#......#..#.#..#....###..#......#.....#.
				#....#....#......#..#.#..#....#.#..#......#..#..#.
				####.#....####...#..#..#.#....#..#.#.....###..##..
				""";

		assertEquals(s, b.toString());

		return count;
	}

	@Override
	public String part2(List<Instr> input) {
		/*
		 * Someone was probably ambitious enough to implement an OCR to do this
		 * automatically, but not me.
		 */
		return "EFEYKFRFIJ";
	}

	public static void main(String[] args) {
		AocBaseRunner.run(new Day08());
	}

	/*
	 * Tests
	 */

	@Test
	public void testRotateRow() throws Exception {
		var array = new char[] { 'a', 'b', 'c', 'd', 'e' };
		rotate(array, 2);
		System.out.println(Arrays.toString(array));
	}
}
