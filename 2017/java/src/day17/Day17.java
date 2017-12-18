package day17;

import static org.junit.Assert.*;

import java.util.LinkedList;

import org.junit.Test;

public class Day17 {

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

	@Test
	public void testShort() throws Exception {
		Spinlock spinlock = new Spinlock();

		spinlock.spinlock(3, 1, 3);
		assertEquals(1, spinlock.getValueAfterPos());

		spinlock.spinlock(3, 4, 4);
		assertEquals(3, spinlock.getValueAfterPos());

		spinlock.spinlock(3, 5, 5);
		assertEquals(2, spinlock.getValueAfterPos());
	}

	@Test
	public void testFull() throws Exception {
		Spinlock spinlock = new Spinlock();
		spinlock.spinlock(377, 1, 2017);
		System.out.println(
				"Day17: value after 2017: " + spinlock.getValueAfterPos());
		assertEquals(596, spinlock.getValueAfterPos());
	}

	@Test
	public void testFull_Part2() throws Exception {
		Spinlock2 spinlock = new Spinlock2();
		spinlock.spinlock(377, 50_000_000);
		System.out.println("Day 17: value after zero (part 2): "
				+ spinlock.getValueAfterZero());
	}
}
