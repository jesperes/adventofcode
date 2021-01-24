package aoc2015;

import java.io.File;
import java.io.IOException;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Optional;
import java.util.Set;

import aoc2015.Day19.InputData;
import common2.AocBaseRunner;
import common2.AocPuzzleInfo;
import common2.AocResult;
import common2.IAocIntPuzzle;
import common2.InputUtils;

public class Day19 implements IAocIntPuzzle<InputData> {

	record InputData(Map<String, String> rules, String input) {

	}

	private Collection<String> getAllReductions(InputData input) {
		Set<String> set = new HashSet<>();
		for (Entry<String, String> e : input.rules.entrySet()) {

			for (int i = 0; i < input.input.length(); i++) {
				int start = i;
				int end = i + e.getValue().length();
				if (end > input.input.length())
					continue;

				String substr = input.input.substring(start, end);
				if (substr.equals(e.getValue())) {
					String s = new StringBuilder(input.input)
							.replace(start, end, e.getKey()).toString();
					set.add(s);
				}
			}
		}
		return set;
	}

	@Override
	public AocPuzzleInfo getInfo() {
		return new AocPuzzleInfo(2015, 19, "Medicine for Rudolph", true);
	}

	@Override
	public AocResult<Integer, Integer> getExpected() {
		return AocResult.of(576, 207);
	}

	@Override
	public InputData parse(Optional<File> file) throws IOException {
		Map<String, String> rules = new HashMap<>();
		String input = "";

		for (String line : InputUtils.asStringList(file.get())) {
			line = line.replace("Rn", "(").replace("Ar", ")").replace("Y", ",");
			String s[] = line.split(" ");
			switch (s.length) {
			case 3:
				rules.put(s[2], s[0]);
				break;
			case 1:
				input = line;
				break;
			}
		}

		return new InputData(rules, input);
	}

	@Override
	public Integer part1(InputData input) {
		return getAllReductions(input).size();
	}

	@Override
	public Integer part2(InputData input) {
		/*
		 * Part 2. The minimum number of steps needed can be computed *without
		 * knowing the actual substitutions*, by simply looking at the rules. I
		 * stole the solution to this from Reddit:
		 * https://www.reddit.com/r/adventofcode/comments/3xflz8/
		 * day_19_solutions/cy4etju.
		 *
		 * The steps variable here is increased by one each element, then
		 * decreased for each rule which can reduce the string by more than one
		 * step. The initial -1 is the length of the start molecule "e".
		 */
		int steps = -1;
		for (char c : input.input.toCharArray()) {
			if (!Character.isLowerCase(c))
				steps++;

			switch (c) {
			case '(':
			case ')':
				steps--;
				break;
			case ',':
				steps -= 2;
				break;
			}
		}
		return steps;
	}

	public static void main(String[] args) {
		AocBaseRunner.run(new Day19());
	}
}
