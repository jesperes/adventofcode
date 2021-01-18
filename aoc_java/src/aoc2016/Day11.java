package aoc2016;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

import org.junit.Test;

import aoc2016.Day11.State;
import common2.AocPuzzleInfo;
import common2.AocResult;
import common2.IAocIntPuzzle;

public class Day11 implements IAocIntPuzzle<State> {

	enum Type {
		GENERATOR, MICROCHIP;
	}

	record Component(Type type, int num, int floor) {
		public String toString() {
			return String.format("%d%s@F%d", num, type.toString().charAt(0), floor);
		}
	}

	record State(List<Component> components, int elevator) {
		boolean isEndState() {
			return components.stream().allMatch(comp -> comp.floor == 4);
		}

		/*
		 * Create a copy of the current state, moving one or more components. If the new
		 * state is valid, add it to the list of states.
		 */
		void move(int newfloor, List<Component> toMove, List<State> states) {
			assertEquals(1, Math.abs(elevator - newfloor)); // elevator can only move one floor at a time
			List<Component> listcopy = new ArrayList<>();
			for (Component c : components) {
				if (toMove.contains(c)) {
					assertEquals(1, Math.abs(c.floor - newfloor));
					assertTrue(newfloor >= 1 && newfloor <= 4);
					listcopy.add(new Component(c.type, c.num, newfloor));
				} else {
					listcopy.add(new Component(c.type, c.num, c.floor));
				}
			}

			var newstate = new State(listcopy, newfloor);
			if (newstate.isValid()) {
				System.out.format("Moved %s to %d%n", toMove, newfloor);
				states.add(newstate);
			}
		}

		private boolean isValid() {
			// Loop over all components to check if any components would
			// be fried if moved to this state.
			for (Component chip : components) {
				if (chip.type == Type.GENERATOR)
					continue;

				// Is microchip shielded?
				boolean isShielded = false;
				for (Component shield : components) {
					if (shield.floor == chip.floor && chip.type == Type.GENERATOR && shield.num == chip.num) {
						isShielded = true;
						break;
					}
				}

				// If it is shielded, we're ok. Proceed to check remaining components.
				if (isShielded)
					continue;

				// If it is not shielded, check if there is generator on the same floor
				// which will fry it.
				for (Component otherGenerator : components) {
					if (otherGenerator.type == Type.GENERATOR && //
							otherGenerator.floor == chip.floor && //
							otherGenerator.num != chip.num) {
						return false;
					}
				}
			}

			// All components are safe.
			return true;
		}

		/**
		 * Returns a collection of possible next states.
		 * 
		 * @return
		 */
		List<State> getNextStates() {
			List<State> states = new ArrayList<>();

			// See
			// https://www.reddit.com/r/adventofcode/comments/5hoia9/2016_day_11_solutions/db1v1ws?utm_source=share&utm_medium=web2x&context=3

			// TODO (2) if floors 1-n are empty, do not move down to floor n.
			// TODO (1) don't bother just moving one item if you can move two

			for (Component a : components) {
				// Can only move components on the elevator floor
				if (a.floor != elevator)
					continue;

				// Move just one component up or down
				if (elevator < 4) {
					move(elevator + 1, List.of(a), states);
				} else if (elevator > 1) {
					move(elevator - 1, List.of(a), states);
				}

				// Move two components up or down
				for (Component b : components) {
					if (b.equals(a) || b.floor != elevator)
						continue;

					if (elevator < 4) {
						move(elevator + 1, List.of(a, b), states);
					} else if (elevator > 1) {
						move(elevator - 1, List.of(a, b), states);
					}
				}
			}

			return states;
		}

		public String toString() {
			int minnr = components.stream().mapToInt(c -> c.num).min().getAsInt();
			int maxnr = components.stream().mapToInt(c -> c.num).max().getAsInt();
			StringBuilder b = new StringBuilder();
			for (int floor = 4; floor >= 1; floor--) {
				b.append(String.format("F%d ", floor));

				if (floor == elevator) {
					b.append(" E ");
				} else {
					b.append(" . ");
				}
				for (int pos = minnr; pos <= maxnr; pos++) {
					if (components.contains(new Component(Type.GENERATOR, pos, floor))) {
						b.append(String.format(" %dG ", pos));
					} else {
						b.append(" .  ");
					}
					if (components.contains(new Component(Type.MICROCHIP, pos, floor))) {
						b.append(String.format(" %dM ", pos));
					} else {
						b.append(" .  ");
					}
				}
				b.append("\n");
			}
			return b.toString();
		}
	}

	@Override
	public AocPuzzleInfo getInfo() {
		return new AocPuzzleInfo(2016, 11, "Radioisotope Thermoelectric Generators", false);
	}

	@Override
	public AocResult<Integer, Integer> getExpected() {
		return AocResult.of(37, 61);
	}

	/**
	 * Rules
	 * 
	 * 1. Microchips without their generators cannot be on the same floor as another
	 * generator.
	 * 
	 * 2. Elevator can hold one or two components (either microchip or generator)
	 * 
	 * 3. When moving components on the elevator, each component in the elevator
	 * must obey rule (1) as if the components where unloaded at that level.
	 * 
	 * Representation
	 * 
	 * 1. We need to be able to generate all possible ways to move components from a
	 * given state.
	 *
	 * @formatter:off
	 * - all moves consisting on moving a single component either up or down, so potentially
	 *   N * 2.
	 * - all moves consisting on moving any pair of components either up or down, so 
	 *   (N * (N - 1) * 2)
	 * @formatter:on
	 * 
	 * This means for 5 components a maximum 10 one-component moves + 180 two-component moves,
	 * but large parts of these are invalid (moving chips to floors where they will be fried,
	 * or moving chips to non-existing floors).
	 * 
	 * 1b. Write code for validating a state.
	 * 
	 * 2. We need to "normalize" a state so that equivalent states becomes
	 * identical. We should be able to do this by "renumbering" components from the
	 * bottom up:
	 * 
	 * @formatter:off
	 * for floor in floors:
	 *     for component in components[floor]:
	 *         skip if already renamed
	 *         id <- take next lowest id
	 *         "rename" component to id (replace both occurrences, maybe on other floors)
	 * @formatter:on
	 * 
	 * This step will allow us to prune the search space significantly.
	 * 
	 * 3. So we will have a function next(S) such that invoking next(S) yields a collection of
	 * new states which are both valid and unique.
	 *  
	 * 4. Breadth-first search to find the shortest number of steps to move all 
	 * components to the top floor. 
	 * 
	 * (We cannot use A* because we don't have an "admissible heuristic", i.e. we cannot 
	 * safely estimate the number of steps from a given state to the end-state such that
	 * we are guaranteed to always choose the "best" next step.)
	 */
	@Override
	public State parse(Optional<File> file) throws IOException {

//		The first floor contains a strontium generator, a strontium-compatible microchip, a plutonium generator, and a plutonium-compatible microchip.
//		The second floor contains a thulium generator, a ruthenium generator, a ruthenium-compatible microchip, a curium generator, and a curium-compatible microchip.
//		The third floor contains a thulium-compatible microchip.
//		The fourth floor contains nothing relevant.

		return new State(List.of( //
				new Component(Type.GENERATOR, 1, 1), // strontium generator
				new Component(Type.MICROCHIP, 1, 1), // strontium microchip
				new Component(Type.GENERATOR, 2, 1), // plutonium generator
				new Component(Type.MICROCHIP, 2, 1), // plutonium microchip
				new Component(Type.GENERATOR, 3, 2), // thulium generator
				new Component(Type.GENERATOR, 4, 2), // ruthenium generator
				new Component(Type.MICROCHIP, 4, 2), // ruthenium microchip
				new Component(Type.GENERATOR, 5, 2), // curium generator
				new Component(Type.MICROCHIP, 5, 2), // curium microchip
				new Component(Type.MICROCHIP, 3, 3) // thulium
		), 1);
	}

	@Override
	public Integer part1(State input) {
		return 0;
	}

	@Override
	public Integer part2(State input) {
		return 0;
	}

	/*
	 * Helpers
	 */

	/*
	 * Tests
	 */

	@Test
	public void testEquals() throws Exception {
		assertEquals(parse(null), parse(null));
	}

	@Test
	public void testNextState() throws Exception {
//		The first floor contains a hydrogen-compatible microchip and a lithium-compatible microchip.
//		The second floor contains a hydrogen generator.
//		The third floor contains a lithium generator.
//		The fourth floor contains nothing relevant.
		var state = new State(List.of(//
				new Component(Type.MICROCHIP, 1, 1), // hydrogen microchip
				new Component(Type.MICROCHIP, 2, 1), // lithium microchip
				new Component(Type.GENERATOR, 1, 2), // hydrogen generator
				new Component(Type.GENERATOR, 2, 3) // lithium generator
		), 1);
		System.out.println(state);
		System.out.println("Next states:");
		for (State s : state.getNextStates()) {
			System.out.println(s);
			System.out.println();
		}
	}
}
