package aoc2015;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Optional;
import java.util.Set;

import com.google.common.collect.Sets;

import common2.AocBaseRunner;
import common2.AocPuzzleInfo;
import common2.AocResult;
import common2.IAocIntPuzzle;
import common2.InputUtils;

public class Day17 implements IAocIntPuzzle<List<Integer>> {

	record Pair(int x, int y) {
	}

	@Override
	public AocPuzzleInfo getInfo() {
		return new AocPuzzleInfo(2015, 17, "No Such Thing as Too Much", true);
	}

	@Override
	public AocResult<Integer, Integer> getExpected() {
		return AocResult.of(1638, 17);
	}

	@Override
	public List<Integer> parse(Optional<File> file) throws IOException {
		return InputUtils.asIntList(file.get());
	}

	@Override
	public Integer part1(List<Integer> input) {
		Set<Pair> buckets = new HashSet<>();
		int n = 1;
		for (int x : input) {
			buckets.add(new Pair(x, n++));
		}

		List<Set<Pair>> combos = new ArrayList<>();

		for (int i = 1; i < buckets.size(); i++) {
			Sets.combinations(buckets, i).stream().filter(
					set -> set.stream().mapToInt(pair -> pair.x).sum() == 150)
					.forEach(combos::add);
		}
		return combos.size();
	}

	@Override
	public Integer part2(List<Integer> input) {
		Set<Pair> buckets = new HashSet<>();
		int n = 1;
		for (int x : input) {
			buckets.add(new Pair(x, n++));
		}

		List<Set<Pair>> combos = new ArrayList<>();

		for (int i = 1; i < buckets.size(); i++) {
			Sets.combinations(buckets, i).stream().filter(
					set -> set.stream().mapToInt(pair -> pair.x).sum() == 150)
					.forEach(combos::add);
		}

		int minSize = combos.stream().mapToInt(set -> set.size()).min()
				.getAsInt();

		return (int) combos.stream().filter(set -> set.size() == minSize)
				.count();
	}

	public static void main(String[] args) {
		AocBaseRunner.run(new Day17());
	}
}
