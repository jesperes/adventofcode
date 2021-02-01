package aoc2017;

import java.io.File;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;
import java.util.Optional;
import java.util.function.Consumer;

import aoc2017.Day25.TuringMachine;
import common2.AocBaseRunner;
import common2.AocPuzzleInfo;
import common2.AocResult;
import common2.IAocIntPuzzle;

public class Day25 implements IAocIntPuzzle<TuringMachine> {

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
	}

	static class State {
		String name;
		Action[] actions;

		public State(String name, Action[] actions) {
			this.name = name;
			this.actions = actions;
		}
	}

	static class Tape {
		Map<Integer, Integer> storage = new HashMap<>();

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
				tape.writeValue(cursor, action.value);
				cursor += action.direction;
				currentState = states.get(action.next.name);
			}
		}
	}

	@Override
	public AocPuzzleInfo getInfo() {
		return new AocPuzzleInfo(2017, 25, "The Halting Problem", false);
	}

	@Override
	public AocResult<Integer, Integer> getExpected() {
		return AocResult.of(2725, null);
	}

	@Override
	public TuringMachine parse(Optional<File> file) throws IOException {
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

	@Override
	public Integer part1(TuringMachine tm) {
		int steps = 12368930;
		tm.run(steps);
		return tm.getDiagnosticChecksum();
	}

	@Override
	public Integer part2(TuringMachine input) {
		return null;
	}

	public static void main(String[] args) {
		AocBaseRunner.run(new Day25());
	}
}
