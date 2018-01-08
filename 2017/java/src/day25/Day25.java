package day25;

import static org.junit.Assert.*;

import java.util.HashMap;
import java.util.Map;
import java.util.SortedMap;
import java.util.TreeMap;
import java.util.function.Consumer;

import org.junit.Test;

public class Day25 {

	final static int DIR_RIGHT = 1;
	final static int DIR_LEFT = -1;

	static class Action {
		final int condition;
		final int value;
		final int direction;
		State next;

		public Action(int condition, int value, int direction) {
			this.condition = condition;
			this.value = value;
			this.direction = direction;
		}

		public void setNext(State next) {
			this.next = next;
		}

		@Override
		public String toString() {
			return String.format(
					"if (val == %d) then val = %d, move %s then goto %s",
					condition, value, direction == DIR_RIGHT ? "right" : "left",
					next.name);
		}

	}

	static class State {
		String name;
		Action[] actions;

		public State(String name, Action[] actions) {
			this.name = name;
			this.actions = actions;
		}

		@Override
		public String toString() {
			StringBuilder builder = new StringBuilder();
			builder.append(String.format("State %s:%n", name));
			for (Action a : actions) {
				builder.append("  " + a);
				builder.append("\n");
			}
			return builder.toString();
		}
	}

	/**
	 * Implement the tape as an ordered map (TreeMap) of all the ones on the
	 * tape.
	 * 
	 * @author jespe
	 */
	static class Tape {

		SortedMap<Integer, Integer> storage = new TreeMap<>();

		public int readValue(int cursor) {
			return storage.getOrDefault(cursor, 0);
		}

		public void writeValue(int cursor, int value) {
			if (value == 0)
				storage.remove(cursor);
			else
				storage.put(cursor, value);
		}

		public void forEach(Consumer<Integer> consumer, int start, int end) {
			for (int i = start; i <= end; i++) {
				consumer.accept(i);
			}
		}

		public String toString(int cursor, int start, int end) {
			StringBuilder builder = new StringBuilder();
			forEach(c -> {
				builder.append(c.equals(cursor) ? "[" : " ");
				builder.append(readValue(c));
				builder.append(c.equals(cursor) ? "]" : " ");
			}, start, end);
			return builder.toString();
		}

		public int getDiagnosticChecksum() {
			return storage.size();
		}
	}

	static class TuringMachine {
		Map<String, State> states = new HashMap<>();
		State currentState;
		Tape tape = new Tape();
		int cursor = 0;
		String initState = "A";

		public TuringMachine(State... states) {
			for (State state : states) {
				this.states.put(state.name, state);
			}

			currentState = this.states.get(initState);
		}

		public int getDiagnosticChecksum() {
			return tape.getDiagnosticChecksum();
		}

		public void wire(String from, int condition, String to) {
			for (Action action : states.get(from).actions) {
				if (action.condition == condition) {
					action.next = states.get(to);
				}
			}
		}

		public Action getStateActionForInput(State currentState, int value) {
			for (Action action : currentState.actions) {
				if (action.condition == value)
					return action;
			}

			throw new AssertionError();
		}

		public void run(int steps) {
			for (int i = 1; i < steps; i++) {
				int value = tape.readValue(cursor);
				Action action = getStateActionForInput(currentState, value);
				// System.out.format("Read value %d at position %s. Action:
				// %s%n",
				// value, cursor, action);
				tape.writeValue(cursor, action.value);
				cursor += action.direction;
				if (!states.containsKey(action.next.name))
					throw new AssertionError(
							"No such state: " + action.next.name);
				currentState = states.get(action.next.name);
			}

			System.out.format("In state %s: %s%n", currentState.name,
					tape.toString(cursor, -5, 5));
		}

		@Override
		public String toString() {
			StringBuilder builder = new StringBuilder();
			builder.append("Turing machine:\n");
			builder.append("Tape: " + tape.toString(cursor, -5, 5) + "\n");
			for (State state : states.values()) {
				builder.append(state);
				builder.append("\n");
			}
			return builder.toString();
		}
	}

	TuringMachine getSmallTestInput() {
		// @formatter:off
		TuringMachine tm = new TuringMachine(
				new State("A", new Action[] { 
						new Action(0, 1, +1),
						new Action(1, 0, -1) 
						}),
				new State("B", new Action[] { 
						new Action(0, 1, -1),
						new Action(1, 1, +1) 
						}));
		// @formatter:on

		tm.wire("A", 0, "B");
		tm.wire("A", 1, "B");
		tm.wire("B", 0, "A");
		tm.wire("B", 1, "A");

		return tm;
	}

	TuringMachine getLargeTestInput() {
		// @formatter:off
		TuringMachine tm = new TuringMachine(
				new State("A", new Action[] { 
						new Action(0, 1, DIR_RIGHT),
						new Action(1, 0, DIR_RIGHT) 
						}),
				new State("B", new Action[] { 
						new Action(0, 0, DIR_LEFT),
						new Action(1, 0, DIR_RIGHT) 
						}),
				new State("C", new Action[] { 
						new Action(0, 1, DIR_RIGHT),
						new Action(1, 1, DIR_RIGHT) 
						}),
				new State("D", new Action[] { 
						new Action(0, 1, DIR_LEFT),
						new Action(1, 0, DIR_LEFT) 
						}),
				new State("E", new Action[] { 
						new Action(0, 1, DIR_RIGHT),
						new Action(1, 1, DIR_LEFT) 
						}),
				new State("F", new Action[] { 
						new Action(0, 1, DIR_RIGHT),
						new Action(1, 1, DIR_RIGHT) 
						})
				);
		// @formatter:on

		tm.wire("A", 0, "B");
		tm.wire("A", 1, "C");
		tm.wire("B", 0, "A");
		tm.wire("B", 1, "D");
		tm.wire("C", 0, "D");
		tm.wire("C", 1, "A");
		tm.wire("D", 0, "E");
		tm.wire("D", 1, "D");
		tm.wire("E", 0, "F");
		tm.wire("E", 1, "B");
		tm.wire("F", 0, "A");
		tm.wire("F", 1, "E");

		return tm;
	}

	@Test
	public void testLargeInput() throws Exception {
		TuringMachine tm = getLargeTestInput();
		int steps = 12368930;
		tm.run(steps);
		int checksum = tm.getDiagnosticChecksum();
		System.out.format("Diagnostic checksum after %d steps: %d%n", steps,
				checksum);
	}

	@Test
	public void testToString() throws Exception {
		TuringMachine tm = getSmallTestInput();
		tm.run(6);
		assertEquals(3, tm.getDiagnosticChecksum());
	}
}
