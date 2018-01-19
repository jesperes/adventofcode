package day11;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Queue;
import java.util.Set;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

public class Day11Alt {
	static public boolean DEBUG = false;

	String inputState = "1/2/1/3/1"; // elevator is first component
	String[] components = new String[] { "E", "HG", "HM", "LG", "LM" };

	Pattern solution = Pattern.compile("^.(/4)+$");

	public boolean isSolution(String state) {
		return solution.matcher(state).matches();
	}

	@SafeVarargs
	final public <T> Set<T> setOf(T... elems) {
		Set<T> set = new HashSet<>();
		for (T e : elems) {
			set.add(e);
		}
		return set;
	}

	public String computeNewState(String input, int newfloor,
			Set<String> componentsToMove) {
		String[] oldstate = input.split("/");
		String[] newstate = new String[components.length];
		newstate[0] = String.valueOf(newfloor);

		for (int i = 1; i < components.length; i++) {
			if (componentsToMove.contains(components[i])) {
				// moving component at index i means assigning it the same
				// index as the elevator
				newstate[i] = newstate[0];
			} else {
				newstate[i] = oldstate[i];
			}
		}

		return Arrays.stream(newstate).collect(Collectors.joining("/"));
	}

	public boolean isValidMove(String input) {
		if (DEBUG) {
			System.out.println("State: " + input);
			System.out.println(showState(input));
		}

		String[] floors = input.split("/");

		// For all microchips
		for (int i = 0; i < components.length; i++) {
			String m = components[i];
			int f_m = Integer.valueOf(floors[i]);

			if (!m.endsWith("M"))
				continue;

			if (DEBUG)
				System.out.format("Checking microchip %s at floor %d%n", m,
						f_m);

			boolean needsShielding = false;
			boolean isShielded = false;

			// For all RTGs
			for (int j = 0; j < components.length; j++) {

				String g = components[j];
				int f_g = Integer.valueOf(floors[j]);

				if (!g.endsWith("G"))
					continue;

				if (DEBUG)
					System.out.format(
							"Checking microchip %s at floor %d against %s%n", m,
							f_m, g);

				if (f_g == f_m) {
					if (g.charAt(0) == m.charAt(0)) {
						// If this generator is compatible, it will shield the
						// chip
						isShielded = true;
						if (DEBUG)
							System.out.format("Chip %s is shielded by %s%n", m,
									g);
					} else {
						// Otherwise, the chip needs shielding.
						needsShielding = true;
						if (DEBUG)
							System.out.format(
									"Chip %s needs to be shielded from %s%n", m,
									g);
					}
				}
			}

			if (needsShielding && !isShielded) {
				if (DEBUG)
					System.out.format(
							"Chip %s needs to be shielded on floor %s but isn't.%n",
							m, f_m);
				return false;
			}
		}

		// All microchips are either shielded or does not need shielding.
		return true;
	}

	public void addValidMove(String input, Set<String> comps, int floor,
			List<String> validMoves) {

		if (floor < 1 || floor > 4)
			return;

		String newstate = computeNewState(input, floor, comps);

		if (isValidMove(newstate)) {
			validMoves.add(newstate);
		}
	}

	public List<String> getMoves(String state) {
		String[] floors = state.split("/");
		int floor = Integer.valueOf(floors[0]);
		Set<String> movable = new HashSet<>();

		for (int i = 1; i <= 4; i++) {
			if (components[i].equals("E") || !floors[i].equals(floors[0]))
				continue;

			movable.add(components[i]);
		}

		List<String> validMoves = new ArrayList<>();

		for (String a : movable) {
			addValidMove(state, setOf(a), floor + 1, validMoves);
			addValidMove(state, setOf(a), floor - 1, validMoves);

			for (String b : movable) {
				if (a.compareTo(b) < 0) {
					addValidMove(state, setOf(a, b), floor + 1, validMoves);
					addValidMove(state, setOf(a, b), floor - 1, validMoves);
				}
			}
		}

		return validMoves;
	}

	public Optional<List<String>> bfs(String init) {
		// States not yet checked.
		Queue<String> openSet = new LinkedList<>();

		// States checked.
		Set<String> closedSet = new HashSet<>();

		// Map to reconstruct the search path.
		Map<String, String> metadata = new HashMap<>();

		openSet.add(init);
		while (!openSet.isEmpty()) {

			System.out.println("Number of unchecked states: " + openSet.size());
			String state = openSet.remove();

			// if (DEBUG)
			System.out.println("Checking state:\n" + showState(state));

			if (isSolution(state)) {
				return Optional.of(constructSolutionList(state, metadata));
			}

			for (String child : getMoves(state)) {
				if (closedSet.contains(child)) {
					continue;
				}

				if (!openSet.contains(child)) {
					metadata.put(child, state);
					openSet.add(child);
				}
			}

			closedSet.add(state);
		}

		return Optional.empty();
	}

	private List<String> constructSolutionList(String state,
			Map<String, String> metadata) {
		List<String> solutions = new ArrayList<>();

		String current = state;
		while (metadata.containsKey(current)) {
			current = metadata.get(current);
			solutions.add(current);
		}

		Collections.reverse(solutions);
		return solutions;
	}

	public String showState(String state) {
		StringBuilder b = new StringBuilder();
		String[] floors = state.split("/");

		for (int i = 4; i >= 1; i--) {
			b.append(String.format("F%d", i));

			for (int ci = 0; ci < components.length; ci++) {
				String comp = components[ci];
				int floor = Integer.valueOf(floors[ci]);
				if (i == floor) {
					b.append(String.format("%3s", comp));
				} else {
					b.append(String.format("%3s", "."));
				}

			}
			b.append("\n");
		}

		return b.toString();
	}
}
