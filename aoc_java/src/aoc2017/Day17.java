package aoc2017;

import java.io.File;
import java.io.IOException;
import java.util.LinkedList;
import java.util.Optional;

import common2.AocBaseRunner;
import common2.AocPuzzleInfo;
import common2.AocResult;
import common2.IAocIntPuzzle;

public class Day17 implements IAocIntPuzzle<Integer> {

	static class Spinlock {
		int pos = 0;
		LinkedList<Integer> buffer = new LinkedList<Integer>();

		Spinlock() {
			buffer.add(pos);
		}

		int getValueAfterPos() {
			return buffer.get((pos + 1) % buffer.size());
		}

		@Override
		public String toString() {
			return "Spinlock [pos=" + pos + ", buffer=" + buffer + "]";
		}

		void spinlock(int steps, int startValue, int stopValue) {
			for (int nextValue = startValue; nextValue <= stopValue; nextValue++) {
				pos = (pos + steps) % buffer.size();
				if (pos == buffer.size() - 1) {
					// If we are at the end of the list, add the next value
					buffer.add(nextValue);
					pos = buffer.size() - 1;
				} else {
					// Otherwise insert it at the position after pos.
					pos++;
					buffer.add(pos, nextValue);
				}
			}
		}
	}

	/*
	 * In this case, we only need to keep track of the number inserted
	 * immediately after 0.
	 */
	static class Spinlock2 {
		int bufferSize;
		int pos;
		int valueAfterZero;

		void spinlock(int steps, int stopValue) {
			bufferSize = 1;
			pos = 0;

			for (int nextValue = 1; nextValue <= stopValue; nextValue++) {
				pos = (pos + steps) % bufferSize;

				// The only interesting case here is when we end up at
				// position 0. In that case, we store the next value, and
				// continue.
				if (pos == 0) {
					valueAfterZero = nextValue;
				}

				pos++;
				bufferSize++;
			}
		}

		int getValueAfterZero() {
			return valueAfterZero;
		}
	}

	@Override
	public AocPuzzleInfo getInfo() {
		return new AocPuzzleInfo(2017, 17, "Spinlock", false);
	}

	@Override
	public AocResult<Integer, Integer> getExpected() {
		return AocResult.of(596, 39051595);
	}

	@Override
	public Integer parse(Optional<File> file) throws IOException {
		return 377;
	}

	@Override
	public Integer part1(Integer input) {
		Spinlock spinlock = new Spinlock();
		spinlock.spinlock(377, 1, 2017);
		return spinlock.getValueAfterPos();
	}

	@Override
	public Integer part2(Integer input) {
		Spinlock2 spinlock = new Spinlock2();
		spinlock.spinlock(377, 50_000_000);
		return spinlock.getValueAfterZero();
	}

	public static void main(String[] args) {
		AocBaseRunner.run(new Day17());
	}
}
