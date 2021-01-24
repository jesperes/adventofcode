package aoc2016;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;

import aoc2016.Day24.Grid;
import common.AStar;
import common.Combinatorics;
import common2.AocBaseRunner;
import common2.AocPuzzleInfo;
import common2.AocResult;
import common2.IAocIntPuzzle;
import common2.InputUtils;

public class Day24 implements IAocIntPuzzle<Grid> {
	record Pos(int x, int y) {
	}

	record Pair(int a, int b) {
	}

	record Grid(Map<Pos, Character> map, Map<Integer, Pos> numbers) {
	}

	@Override
	public AocPuzzleInfo getInfo() {
		return new AocPuzzleInfo(2016, 24, "Air Duct Spelunking", true);
	}

	@Override
	public AocResult<Integer, Integer> getExpected() {
		return AocResult.of(462, 676);
	}

	@Override
	public Grid parse(Optional<File> file) throws IOException {
		Map<Pos, Character> map = new HashMap<>();
		Map<Integer, Pos> numbers = new HashMap<>();
		int y = 0;
		for (String line : InputUtils.asStringList(file.get())) {
			int x = 0;
			for (char c : line.toCharArray()) {
				var pos = new Pos(x++, y);
				if (c == '#')
					map.put(pos, c);
				else if (Character.isDigit(c))
					numbers.put(c - '0', pos);
			}
			y++;
		}
		return new Grid(map, numbers);
	}

	@Override
	public Integer part1(Grid input) {
		return shortestPath(input, false);
	}

	@Override
	public Integer part2(Grid input) {
		return shortestPath(input, true);
	}

	private Integer shortestPath(Grid input, boolean part2) {
		Map<Pair, Integer> distances = calculateDistances(input);

		List<Integer> numbers = new ArrayList<>();
		numbers.addAll(input.numbers.keySet());
		numbers.removeIf(n -> n.equals(0));
		int mindist = Integer.MAX_VALUE;

		for (List<Integer> permutation : Combinatorics.permutations(numbers)) {
			int from = 0;
			int dist = 0;
			for (int b : permutation) {
				dist += distances.get(new Pair(from, b));
				from = b;
			}

			// For part 2, we need to go back to 0.
			if (part2)
				dist += distances.get(new Pair(from, 0));

			if (dist < mindist) {
				mindist = dist;
			}
		}

		return mindist;
	}

	private Map<Pair, Integer> calculateDistances(Grid input) {
		Map<Pair, Integer> distances = new HashMap<>();

		for (int a : input.numbers.keySet()) {
			for (int b : input.numbers.keySet()) {
				if (a == b)
					continue;

				Pair reverse = new Pair(b, a);
				int dist;
				if (distances.containsKey(reverse)) {
					dist = distances.get(reverse);
				} else {
					dist = distance(input.numbers.get(a), input.numbers.get(b),
							input.map);
				}
				distances.put(new Pair(a, b), dist);
			}
		}
		return distances;
	}

	int distance(Pos from, Pos to, Map<Pos, Character> map) {
		return AStar.astar(from, to, node -> heuristic(node, to),
				node -> neighbors(node, map)).size();
	}

	Collection<Pos> neighbors(Pos node, Map<Pos, Character> map) {
		List<Pos> list = new ArrayList<>();
		addIfOpen(list, map, new Pos(node.x, node.y - 1));
		addIfOpen(list, map, new Pos(node.x + 1, node.y));
		addIfOpen(list, map, new Pos(node.x, node.y + 1));
		addIfOpen(list, map, new Pos(node.x - 1, node.y));
		return list;
	}

	private void addIfOpen(List<Pos> list, Map<Pos, Character> map, Pos pos) {
		if (map.getOrDefault(pos, '.') != '#') {
			list.add(pos);
		}
	}

	int heuristic(Pos from, Pos to) {
		return Math.abs(from.x - to.x) + Math.abs(from.y - to.y);
	}

	public static void main(String[] args) {
		AocBaseRunner.run(new Day24());
	}
}
