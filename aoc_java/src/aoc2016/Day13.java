package aoc2016;

import java.io.File;
import java.io.IOException;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Optional;
import java.util.Set;

import common.AStar;
import common2.AocBaseRunner;
import common2.AocPuzzleInfo;
import common2.AocResult;
import common2.IAocIntPuzzle;

public class Day13 implements IAocIntPuzzle<Integer> {

	static final int INPUT = 1364;

	static char[][] grid = new char[100][100];

	static char readGrid(int x, int y) {
		if (x < 0 || y < 0)
			return '#';

		if (grid[x][y] == 0) {
			int val = (x * x) + (3 * x) + (2 * x * y) + y + (y * y) + INPUT;
			if (Integer.bitCount(val) % 2 == 0) {
				grid[x][y] = '.';
			} else {
				grid[x][y] = '#';
			}
		}

		return grid[x][y];
	}

	record Pos(int x, int y) {

	}

	@Override
	public AocPuzzleInfo getInfo() {
		return new AocPuzzleInfo(2016, 13, "A Maze of Twisty Little Cubicles",
				false);
	}

	@Override
	public AocResult<Integer, Integer> getExpected() {
		return AocResult.of(86, 127);
	}

	@Override
	public Integer parse(Optional<File> file) throws IOException {
		return 0;
	}

	/*
	 * Part 1: A* search.
	 */

	@Override
	public Integer part1(Integer input) {
		var goal = new Pos(31, 39);
		return AStar.astar(new Pos(1, 1), new Pos(31, 39),
				node -> heuristic(node, goal), this::neighbors).size();
	}

	private int heuristic(Pos node, Pos goal) {
		return Math.abs(node.x - goal.x) + Math.abs(node.y - goal.y);
	}

	/*
	 * Part 2: Plain depth-first search, keeping track of which depth we have
	 * seen each node (otherwise we may miss nodes if the first time we see them
	 * isn't the shortest way to them).
	 */

	@Override
	public Integer part2(Integer input) {
		var map = findToDepth(new Pos(1, 1), 50);
		return map.size();
	};

	private Map<Pos, Integer> findToDepth(Pos pos, int maxDepth) {
		Map<Pos, Integer> map = new HashMap<>();
		map.put(pos, 0);
		findToDepth(pos, 1, maxDepth, map);
		return map;
	}

	private void findToDepth(Pos pos, int depth, int maxDepth,
			Map<Pos, Integer> map) {
		if (depth > maxDepth)
			return;

		for (Pos nbr : neighbors(pos)) {
			int nbrDepth = map.getOrDefault(nbr, Integer.MAX_VALUE);
			if (nbrDepth > depth) {
				map.put(nbr, depth);
				findToDepth(nbr, depth + 1, maxDepth, map);
			}
		}
	}

	/*
	 * Helpers
	 */

	private Collection<Pos> neighbors(Pos current) {
		Set<Pos> nbrs = new HashSet<>();
		var x = current.x;
		var y = current.y;

		if (readGrid(x - 1, y) == '.')
			nbrs.add(new Pos(x - 1, y));
		if (readGrid(x + 1, y) == '.')
			nbrs.add(new Pos(x + 1, y));
		if (readGrid(x, y - 1) == '.')
			nbrs.add(new Pos(x, y - 1));
		if (readGrid(x, y + 1) == '.')
			nbrs.add(new Pos(x, y + 1));

		return nbrs;
	}

	public static void main(String[] args) {
		AocBaseRunner.run(new Day13());
	}

}