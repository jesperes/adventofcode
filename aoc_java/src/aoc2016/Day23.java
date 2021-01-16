package aoc2016;

import java.io.File;
import java.io.IOException;
import java.util.List;
import java.util.Optional;

import common2.AocBaseRunner;
import common2.AocPuzzleInfo;
import common2.AocResult;
import common2.IAocIntPuzzle;
import common2.InputUtils;

/**
 * In this puzzle, we are given a program in a Turing Tar-pit type program, and
 * we have to figure out what it really does. It turns out that the program
 * implements a simple factorial (obfuscated by an offset), but without having a
 * multiplication instruction (instead it uses "add").
 */
public class Day23 implements IAocIntPuzzle<List<String>> {

	int factorial(int x) {
		return x == 1 ? 1 : x * factorial(x - 1);
	}

	int prog(int seed) {
		int offset = 70 * 87;
		return offset + factorial(seed);
	}

	@Override
	public AocPuzzleInfo getInfo() {
		return new AocPuzzleInfo(2016, 23, "Safe Cracking", true);
	}

	@Override
	public AocResult<Integer, Integer> getExpected() {
		return AocResult.of(11130, 479007690);
	}

	@Override
	public List<String> parse(Optional<File> file) throws IOException {
		return InputUtils.asStringList(file.get());
	}

	@Override
	public Integer part1(List<String> input) {
		return prog(7);
	}

	@Override
	public Integer part2(List<String> input) {
		return prog(12);
	}

	public static void main(String[] args) {
		AocBaseRunner.run(new Day23());
	}
}
