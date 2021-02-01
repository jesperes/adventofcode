package aoc2017;

import java.io.File;
import java.io.IOException;
import java.util.Arrays;
import java.util.Optional;
import java.util.Random;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

import common2.AocBaseRunner;
import common2.AocPuzzleInfo;
import common2.AocResult;
import common2.IAocIntPuzzle;
import common2.InputUtils;

public class Day21 implements IAocIntPuzzle<String[]> {

	static Random random = new Random(42);
	static String START_PATTERN = ".#./..#/###";

	private static char[][] patternToMatrix(String pattern) {
		String[] rows = pattern.split("/");
		char[][] matrix = new char[rows.length][];
		int rowidx = 0;
		for (String row : rows) {
			matrix[rowidx] = row.toCharArray();
			if (matrix[rowidx].length != rows.length)
				throw new AssertionError("Incorrect row length");
			rowidx++;
		}
		return matrix;
	}

	private static String matrixToPattern(char[][] matrix) {
		return Arrays.stream(matrix).map(row -> new String(row))
				.collect(Collectors.joining("/"));
	}

	private static char[][] clone(char matrix[][]) {
		return patternToMatrix(matrixToPattern(matrix));
	}

	static void rotate(char mat[][]) {
		int N = mat.length;

		for (int x = 0; x < N / 2; x++) {
			for (int y = x; y < N - x - 1; y++) {
				char temp = mat[x][y];
				mat[x][y] = mat[y][N - 1 - x];
				mat[y][N - 1 - x] = mat[N - 1 - x][N - 1 - y];
				mat[N - 1 - x][N - 1 - y] = mat[N - 1 - y][x];
				mat[N - 1 - y][x] = temp;
			}
		}
	}

	static void flipHoriz(char mat[][]) {
		for (int i = 0; i < mat.length; i++) {
			mat[i] = new StringBuilder(new String(mat[i])).reverse().toString()
					.toCharArray();
		}
	}

	static void flipVert(char mat[][]) {
		for (int i = 0; i < mat.length / 2; i++) {
			char[] tmp = mat[i];
			mat[i] = mat[mat.length - i - 1];
			mat[mat.length - i - 1] = tmp;
		}
	}

	static boolean isEqual(char a[][], char b[][]) {
		if (a.length != b.length)
			return false;

		for (int row = 0; row < a.length; row++) {
			if (a[row].length != b[row].length)
				return false;

			for (int col = 0; col < a.length; col++) {
				if (a[row][col] != b[row][col])
					return false;
			}
		}

		return true;
	}

	public static boolean transformAndCheckMatrix(char[][] matrix,
			char[][] pattern, boolean flipH, boolean flipV, int rotateCcw) {

		char[][] m = clone(matrix);

		if (flipH)
			flipHoriz(m);

		if (flipV)
			flipVert(m);

		for (int i = 0; i < rotateCcw; i++) {
			rotate(m);
		}

		return isEqual(m, pattern);
	}

	// Transform a matrix at random
	public char[][] transformRandom(char[][] matrix) {
		char[][] m = clone(matrix);

		IntStream.range(0, random.nextInt(2)).forEach((i) -> {
			flipHoriz(m);
		});

		IntStream.range(0, random.nextInt(2)).forEach((i) -> {
			flipVert(m);
		});

		IntStream.range(0, random.nextInt(4)).forEach((i) -> {
			rotate(m);
		});

		return m;
	}

	public static boolean checkPatternMatch(char[][] matrix, char[][] pattern) {

		// identity
		if (transformAndCheckMatrix(matrix, pattern, false, false, 0))
			return true;

		// rotate ccw * 1
		if (transformAndCheckMatrix(matrix, pattern, false, false, 1))
			return true;

		// rotate ccw * 2
		if (transformAndCheckMatrix(matrix, pattern, false, false, 2))
			return true;

		// rotate ccw * 3
		if (transformAndCheckMatrix(matrix, pattern, false, false, 3))
			return true;

		// flip horiz
		if (transformAndCheckMatrix(matrix, pattern, true, false, 0))
			return true;

		// flip horiz + rotate ccw * 1
		if (transformAndCheckMatrix(matrix, pattern, true, false, 1))
			return true;

		// flip vert
		if (transformAndCheckMatrix(matrix, pattern, false, true, 0))
			return true;

		// flip vert + rotate ccw * 1
		if (transformAndCheckMatrix(matrix, pattern, false, true, 1))
			return true;

		return false;
	}

	private static char[][] transformMatrix(char[][] matrix, String[] rules) {
		for (String rule : rules) {
			String[] elems = rule.split("=>");
			String patstr = elems[0].trim();
			String replacement = elems[1].trim();

			char[][] pattern = patternToMatrix(patstr);

			if (checkPatternMatch(matrix, pattern)) {
				return patternToMatrix(replacement);
			}
		}

		throw new AssertionError(
				"No pattern found matching: " + matrixToPattern(matrix));
	}

	private int getPixels(char[][] matrix) {
		int count = 0;
		for (int j = 0; j < matrix.length; j++) {
			for (int k = 0; k < matrix.length; k++) {
				if (matrix[j][k] == '#')
					count++;
			}
		}

		return count;
	}

	private static char[][] getSubMatrix(char[][] matrix, int row, int col,
			int size) {

		char[][] submatrix = new char[size][];

		for (int r = row; r < row + size; r++) {
			submatrix[r - row] = new char[size];
			for (int c = col; c < col + size; c++) {
				submatrix[r - row][c - col] = matrix[r][c];
			}
		}

		return submatrix;
	}

	// Copy a submatrix into a larger matrix at the given coordinates
	private static void copySubMatrix(char[][] matrix, char[][] submatrix,
			int row, int col) {
		for (int r = 0; r < submatrix.length; r++) {
			for (int c = 0; c < submatrix.length; c++) {
				matrix[row + r][col + c] = submatrix[r][c];
			}
		}
	}

	private static char[][] fractalArt(String startPattern, String[] inputRules,
			int steps) {
		char[][] matrix = patternToMatrix(startPattern);

		for (int i = 0; i < steps; i++) {
			// Split the matrix into submatrices

			// The size of each submatrix to split
			int subsize = (matrix.length % 2 == 0) ? 2 : 3;

			// The number of submatrices
			int numSubMatrices = matrix.length / subsize;

			// The new size of the submatrices. 2x2 -> 3x3, 3x3 -> 4x4.
			int newSubSize = subsize + 1;

			char[][] newmatrix = new char[numSubMatrices * newSubSize][];
			for (int j = 0; j < newmatrix.length; j++) {
				newmatrix[j] = new char[newmatrix.length];
				for (int k = 0; k < newmatrix.length; k++) {
					newmatrix[j][k] = '?';
				}
			}

			int ri = 0;
			for (int row = 0; row < matrix.length; row += subsize, ri++) {
				int ci = 0;
				for (int col = 0; col < matrix.length; col += subsize, ci++) {
					// Extract the submatrix we want to match
					char[][] submatrix = getSubMatrix(matrix, row, col,
							subsize);

					// Transform it according to the rules
					char[][] newsubmatrix = transformMatrix(submatrix,
							inputRules);

					// Copy the matrix into the new (larger) matrix
					copySubMatrix(newmatrix, newsubmatrix, ri * newSubSize,
							ci * newSubSize);
				}
			}

			matrix = newmatrix;
		}

		return matrix;
	}

	@Override
	public AocPuzzleInfo getInfo() {
		return new AocPuzzleInfo(2017, 21, "Fractal Art", true);

	}

	@Override
	public AocResult<Integer, Integer> getExpected() {
		return AocResult.of(205, 3389823);
	}

	@Override
	public String[] parse(Optional<File> file) throws IOException {
		return InputUtils.asStringList(file.get()).stream()
				.toArray(n -> new String[n]);
	}

	@Override
	public Integer part1(String[] rules) {
		char[][] art = fractalArt(START_PATTERN, rules, 5);
		return getPixels(art);
	}

	@Override
	public Integer part2(String[] rules) {
		char[][] art = fractalArt(START_PATTERN, rules, 18);
		return getPixels(art);
	}

	public static void main(String[] args) {
		AocBaseRunner.run(new Day21());
	}

}
