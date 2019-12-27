package aoc2017;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.util.Arrays;
import java.util.Random;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

import org.junit.Test;

import common.AocPuzzle;

public class Day21 extends AocPuzzle {

    public Day21() {
        super(2017, 21);
    }

    static Random random = new Random(42);
    static String START_PATTERN = ".#./..#/###";
    static String[] RULES = new String[] { //
            "../.# => ##./#../...", //
            ".#./..#/### => #..#/..../..../#..#" //
    };

//    private static void printGrid(String pattern) {
//        System.out.println("--");
//        for (String row : pattern.split("/")) {
//            System.out.println(row);
//        }
//    }

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

    /**
     * Shamelessly stolen from
     * https://www.geeksforgeeks.org/inplace-rotate-square-matrix-by-90-degrees/
     *
     * An Inplace function to rotate a N x N matrix by 90 degrees in
     * anti-clockwise direction
     */
    static void rotate(char mat[][]) {
        int N = mat.length;

        // Consider all squares one by one
        for (int x = 0; x < N / 2; x++) {
            // Consider elements in group of 4 in
            // current square
            for (int y = x; y < N - x - 1; y++) {
                // store current cell in temp variable
                char temp = mat[x][y];

                // move values from right to top
                mat[x][y] = mat[y][N - 1 - x];

                // move values from bottom to right
                mat[y][N - 1 - x] = mat[N - 1 - x][N - 1 - y];

                // move values from left to bottom
                mat[N - 1 - x][N - 1 - y] = mat[N - 1 - y][x];

                // assign temp to left
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

    /*
     * The following possible permutations of a 2x2 matrix exist, but not
     * all of them can be reached by rotating/flipping. Note that order is
     * important: flip horiz (0 or more), then flip vertical (0 or more), then
     * rotate (0 or more).
     *
     * We observe that there are 8 distinct rotations. If we write the
     * the 2x2 matrix on a piece of paper, these correspond to 4 positions
     * on each page, with 4 rotations on each page. We could of course do without
     * one of the flips and replace them with rotations.
     *
     * @formatter:off
     * {{1, 2, 3, 4}, // identity
     *  {1, 2, 4, 3}, --
     *  {1, 3, 2, 4}, // flip horiz + rotate ccw * 1
     *  {1, 3, 4, 2}, --
     *  {1, 4, 2, 3}, --
     *  {1, 4, 3, 2}, --
     *  {2, 1, 3, 4}, --
     *  {2, 1, 4, 3}, // flip horiz
     *  {2, 3, 1, 4}, --
     *  {2, 3, 4, 1}, --
     *  {2, 4, 1, 3}, // rotate ccw * 1
     *  {2, 4, 3, 1}, --
     *  {3, 1, 2, 4}, --
     *  {3, 1, 4, 2}, // rotate ccw * 3
     *  {3, 2, 1, 4}, --
     *  {3, 2, 4, 1}, --
     *  {3, 4, 1, 2}, // flip vert
     *  {3, 4, 2, 1}, --
     *  {4, 1, 2, 3}, --
     *  {4, 1, 3, 2}, --
     *  {4, 2, 1, 3}, --
     *  {4, 2, 3, 1}, // flip vert + rotate ccw * 1
     *  {4, 3, 1, 2}, --
     *  {4, 3, 2, 1}} // rotate ccw * 2
     * @formatter:on
     *
     */

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

//            System.out.format(
//                    "[%d] Splitting %dx%d matrix of %dx%d submatrices of size %dx%d -> %dx%d matrix of %dx%d submatrices of size %dx%d%n",
//                    i, matrix.length, matrix.length, //
//                    numSubMatrices, numSubMatrices, //
//                    subsize, subsize, //
//                    newmatrix.length, newmatrix.length, numSubMatrices,
//                    numSubMatrices, //
//                    newSubSize, newSubSize);

            // printGrid(matrixToPattern(matrix));

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
            // printGrid(matrixToPattern(matrix));
        }

        return matrix;
    }

    @Test
    public void testRotate() throws Exception {
        String pat = "..#../#.#.#/..###/#.##./..#..";
        char[][] mat = patternToMatrix(pat);
        rotate(mat);
        rotate(mat);
        rotate(mat);
        rotate(mat);
        assertEquals(pat, matrixToPattern(mat));
    }

    @Test
    public void testFlipHoriz() throws Exception {
        String pat = "..#../#.#.#/..###/#.##./..#..";
        char[][] mat = patternToMatrix(pat);
        flipHoriz(mat);
        flipHoriz(mat);
        assertEquals(pat, matrixToPattern(mat));
    }

    @Test
    public void testFlipVert() throws Exception {
        String pat = "..#../#.#.#/..###/#.##./..#..";
        char[][] mat = patternToMatrix(pat);
        flipVert(mat);
        flipVert(mat);
        assertEquals(pat, matrixToPattern(mat));
    }

    @Test
    public void testFlipRotate() throws Exception {
        String pat = "..#../#.#.#/..###/#.##./..#..";
        char[][] mat = patternToMatrix(pat);
        flipVert(mat);
        rotate(mat);
        flipVert(mat);
        rotate(mat);
        assertEquals(pat, matrixToPattern(mat));
    }

    @Test
    public void testMatches() throws Exception {
        String pat = "..#../#.#.#/..###/#.##./..#..";
        char[][] mat = patternToMatrix(pat);

        // Transform the matrix at random, and make sure that it matches.
        for (int i = 0; i < 100; i++) {
            char[][] randomTransformedMatrix = transformRandom(mat);
            assertTrue(checkPatternMatch(randomTransformedMatrix, mat));
        }
    }

    @Test
    public void testGetSubMatrix() throws Exception {
        char[][] submatrix = getSubMatrix(
                patternToMatrix("#..#/..../..../#..#"), 2, 2, 2);
        assertEquals("../.#", matrixToPattern(submatrix));
    }

    @Test
    public void testTransform() throws Exception {
        char[][] matrix = patternToMatrix(START_PATTERN);
        char[][] matrix2 = transformMatrix(matrix, RULES);
        assertEquals("#..#/..../..../#..#", matrixToPattern(matrix2));
    }

    @Test
    public void testFractalArtSmall() throws Exception {
        char[][] art = fractalArt(START_PATTERN, RULES, 2);
        // printGrid(matrixToPattern(art));
        assertEquals(12, getPixels(art));
    }

    @Test
    public void testLargeInput() throws Exception {
        String[] rules = getInputAsLines().toArray(n -> new String[n]);
        char[][] art = fractalArt(START_PATTERN, rules, 5);
        assertEquals(205, getPixels(art));
        // printGrid(matrixToPattern(art));

    }

    @Test
    public void testLargeInput_Part2() throws Exception {
        String[] rules = getInputAsLines().toArray(n -> new String[n]);
        char[][] art = fractalArt(START_PATTERN, rules, 18);
        // printGrid(matrixToPattern(art));
        assertEquals(3389823, getPixels(art));
    }
}
