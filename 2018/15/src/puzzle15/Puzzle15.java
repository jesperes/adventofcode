package puzzle15;

import java.io.BufferedReader;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;
import java.util.stream.Collectors;

public class Puzzle15 {
	private static class Pos implements Comparable<Pos> {
		final int x;
		final int y;

		public Pos(int x, int y) {
			super();
			this.x = x;
			this.y = y;
		}

		@Override
		public String toString() {
			return String.format("(%d,%d)", x, y);
		}

		public boolean equals(Object o) {
			Pos p = (Pos) o;
			return p.x == x && p.y == y;
		}

		public int hashCode() {
			return Integer.hashCode(x) ^ Integer.hashCode(y);
		}

		@Override
		public int compareTo(Pos o) {
			/*
			 * Sort positions by reading order: top-to-bottom then left-to-right
			 */
			if (y != o.y) {
				return Integer.compare(y, o.y);
			} else {
				return Integer.compare(x, o.x);
			}
		}
	}

	public static void main(String[] args)
			throws FileNotFoundException, IOException {
		char[][] grid = parseInput("testinput2.txt");
		executeRound(grid);
	}

	private static void printGrid(char[][] grid, List<Pos> plist, char plistc) {
		int height = grid.length;
		int width = grid[0].length;

		for (int y = 0; y < height; y++) {
			for (int x = 0; x < width; x++) {
				if (plist.contains(new Pos(x, y))) {
					System.out.print(plistc);
				} else {
					System.out.print(grid[y][x]);
				}
			}
			System.out.println();
		}
	}

	private static char[][] parseInput(String filename)
			throws FileNotFoundException, IOException {
		try (BufferedReader r = new BufferedReader(new FileReader(filename))) {
			List<String> lines = r.lines().collect(Collectors.toList());

			int height = lines.size();
			int width = lines.get(0).length();

			char[][] grid = new char[width][width];
			for (int y = 0; y < height; y++) {
				for (int x = 0; x < width; x++) {
					grid[y][x] = lines.get(y).charAt(x);
				}
			}

			return grid;
		}
	}

	private static List<Pos> getUnitsInOrder(char[][] grid, char... type) {
		List<Pos> list = new ArrayList<>();
		for (int y = 0; y < grid.length; y++) {
			for (int x = 0; x < grid[y].length; x++) {
				char c = grid[y][x];
				for (char t : type) {
					if (c == t)
						list.add(new Pos(x, y));
				}
			}
		}
		return list;

	}

	private static char enemy(char c) {
		if (c == 'G')
			return 'E';
		else
			return 'G';
	}

	// Return the open, adjacents squares to the positions in p.

	private static List<Pos> adjacent(char[][] grid, Pos pos) {
		List<Pos> plist = new ArrayList<>();
		plist.add(pos);
		return adjacent(grid, plist);
	}

	private static List<Pos> adjacent(char[][] grid, List<Pos> plist) {
		List<Pos> list = new ArrayList<>();
		adjacent(grid, plist, list);
		return list;
	}

	private static void adjacent(char[][] grid, List<Pos> plist,
			List<Pos> list) {
		for (Pos p : plist) {
			adjacentToUnit(grid, list, p);
		}
	}

	private static void addIfOpen(char[][] grid, List<Pos> list, int x, int y) {
		if (grid[y][x] == '.') {
			list.add(new Pos(x, y));
		}
	}

	private static void adjacentToUnit(char[][] grid, List<Pos> list, Pos p) {
		// Add all positions which are adjacent to p to the given list.
		addIfOpen(grid, list, p.x, p.y - 1);
		addIfOpen(grid, list, p.x, p.y + 1);
		addIfOpen(grid, list, p.x + 1, p.y);
		addIfOpen(grid, list, p.x - 1, p.y);
	}

	private static void executeRound(char[][] grid) {
		printGrid(grid, Collections.emptyList(), 'c');

		List<Pos> units = getUnitsInOrder(grid, 'E' /* , 'G' */);

		for (Pos unit : units) {
			char type = grid[unit.y][unit.x];
			char enemy = enemy(type);
			List<Pos> enemyUnits = getUnitsInOrder(grid, enemy);

			/*
			 * 1. Move
			 */

			List<Pos> adjacentToEnemyUnits = adjacent(grid, enemyUnits);

			System.out.println("Moving unit at: " + unit);
			System.out.println("Enemy units at: " + enemyUnits);

			for (Pos adjacentToEnemyUnit : adjacentToEnemyUnits) {
				System.out.println(
						"Adjacent to enemy unit: " + adjacentToEnemyUnit);
			}

			// Find all positions reachable from this unit.
			Set<Pos> reachable = new TreeSet<>();
			addReachable(grid, unit, reachable);

			// Filter out all positions which are not adjacent to an enemy
			// position.
			Set<Pos> reachableAndAdjacent = reachable.stream()
					.filter(pos -> adjacentToEnemyUnits.contains(pos))
					.collect(Collectors.toSet());

			getNearest(grid, unit, reachableAndAdjacent);

			printGrid(grid, new ArrayList<>(reachableAndAdjacent), '@');
		}
	}

	private static void getNearest(char[][] grid, Pos unit,
			Set<Pos> reachableAndAdjacent) {
	}

	private static void addReachable(char[][] grid, Pos unit,
			Set<Pos> reachable) {

		for (Pos adjacentUnit : adjacent(grid, unit)) {
			if (reachable.contains(adjacentUnit)) {
				continue;
			} else {
				reachable.add(adjacentUnit);
				addReachable(grid, adjacentUnit, reachable);
			}
		}
	}
}
