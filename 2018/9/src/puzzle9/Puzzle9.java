package puzzle9;

import static org.junit.Assert.assertEquals;

import java.util.Deque;
import java.util.LinkedList;
import java.util.concurrent.TimeUnit;

public class Puzzle9 {

	public static void main(String[] args) {
		// int[] array = new int[] { 1, 2, 3, 4, 5, 0, 0, 0, 0 };
		// int arrSize = 5;
		// System.out.println(Arrays.toString(array));
		// insert(array, 2, 5, arrSize);
		// arrSize++;
		// System.out.println(Arrays.toString(array));
		// assertEquals(3, remove(array, 3, arrSize));
		// System.out.println(Arrays.toString(array));

		assertEquals(32, marbleGameQueue(9, 25));
		assertEquals(8317, marbleGameQueue(10, 1618));
		assertEquals(423717, marbleGameQueue(419, 72164));
		marbleGameQueue(419, 72164 * 100);
	}
//
//	static int marbleGame(int numPlayers, int lastMarble) {
//		long t0 = System.nanoTime();
//		List<Integer> ring = new ArrayList<>();
//		ring.add(0);
//		Map<Integer, Integer> scores = new HashMap<>();
//		int currentPos = 0;
//
//		for (int n = 1; n <= lastMarble; n++) {
//
////			if (n % 100000 == 0) {
////				double progress = ((double) n) / lastMarble;
////				System.out.format("Progress %g%%%n", progress * 100);
////			}
//			if (n % 23 == 0) {
//				int player = n % numPlayers;
//				int removalPos = ((currentPos - 7) + ring.size()) % ring.size();
//				int removed = ring.remove(removalPos);
//				int score = n + removed;
//				scores.compute(player, (k, v) -> v == null ? score : v + score);
//				currentPos = removalPos;
//				if (currentPos >= ring.size())
//					currentPos = 0; // wrap around
//			} else {
//				int insertionPos = (currentPos + 2) % ring.size();
//				ring.add(insertionPos, n);
//				currentPos = insertionPos;
//			}
//		}
//
//		int max = scores.values().stream().mapToInt(v -> v).max().getAsInt();
//		long elapsed = System.nanoTime() - t0;
//
//		System.out.format("Max score for %d players, last marble value %d, time = %d ms%n", numPlayers, lastMarble,
//				TimeUnit.NANOSECONDS.toMillis(elapsed));
//		return max;
//	}

	static <T> void rotateCW(Deque<T> ring, int n) {
		for (int i = 0; i < n; i++) {
			T last = ring.removeLast();
			ring.addFirst(last);
		}
	}

	static <T> void rotateCCW(Deque<T> ring, int n) {
		for (int i = 0; i < n; i++) {
			T first = ring.removeFirst();
			ring.addLast(first);
		}
	}

	static long marbleGameQueue(int numPlayers, int lastMarble) {
		long t0 = System.nanoTime();
		// We rotate the ring so that ring[0] is the current position
		Deque<Integer> ring = new LinkedList<>();
		ring.add(0);
		long[] scores = new long[numPlayers];

		for (int n = 1; n <= lastMarble; n++) {
			if (n % 23 == 0) {
				for (int i = 0; i < 7; i++) {
					ring.addFirst(ring.removeLast());
				}
				int removed = ring.removeFirst();
				scores[n % numPlayers] += n + removed;
			} else {
				// rotate two steps so that the inserted marble ends up in the right location
				ring.addLast(ring.removeFirst());
				ring.addLast(ring.removeFirst());
				ring.addFirst(n);
			}
		}

		long max = 0;
		for (long n : scores) {
			if (n > max)
				max = n;
		}
		long elapsed = System.nanoTime() - t0;

		System.out.format("Max score for %d players == %d, last marble value %d, time = %d ms%n", numPlayers, max,
				lastMarble, TimeUnit.NANOSECONDS.toMillis(elapsed));
		return max;
	}
}
