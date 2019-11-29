package aoc2018;

import static org.junit.Assert.assertEquals;

import java.util.Deque;
import java.util.LinkedList;

import org.junit.Test;

public class Day09 {

    @Test
    public void testExample1() {
        assertEquals(32, marbleGameQueue(9, 25));
    }

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
                // rotate two steps so that the inserted marble ends up in the
                // right location
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
        return max;
    }
}
