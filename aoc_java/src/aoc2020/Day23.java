package aoc2020;

import static org.junit.Assert.assertEquals;

import java.util.Arrays;
import java.util.List;
import java.util.ListIterator;

import org.junit.Test;

import com.google.common.collect.BiMap;

/**
 * AoC puzzle template.
 */
public class Day23 {

    private Object solve(boolean part2, int moves, boolean useTestInput) {

        BiMap<Integer, Integer> list = new BiMap<>();
//        LinkedList<Integer> list = new LinkedList<>();

        list.addAll(useTestInput ? //
                Arrays.asList(3, 8, 9, 1, 2, 5, 4, 6, 7) : //
                Arrays.asList(5, 8, 6, 4, 3, 9, 1, 7, 2));

        if (part2) {
            for (int i = 10; i <= 1_000_000; i++) {
                list.add(i);
            }
        }
        int maxn = list.size();

        // Keep the list such that the current cup is always at the front.
        for (int i = 1; i <= moves; i++) {
            if (i % 100 == 0) {
                System.out.format("Move: %d\n", i);
            }

            int current = list.get(0);
            List<Integer> picked = list.subList(1, 4);

            // System.out.format("L = %s\n", list);
            System.out.format("Current = %d\n", current);
            System.out.format("Picked = %s\n", picked);

            for (int d = current - 1; true; d--) {
                System.out.format("Checking next dest: %d\n", d);
                if (d < 1) {
                    System.out.format("Wrap around to %d\n", maxn);
                    d = maxn;
                }

                boolean found = false;

                ListIterator<Integer> it = list.listIterator(4);
                while (it.hasNext()) {
                    System.out.format("  > %d\n", it.nextIndex());

                    if (it.next() == d) {

                        // Insert picked cards
                        list.addAll(it.nextIndex(), picked);

                        // Remove picked cards
                        list.remove(1);
                        list.remove(1);
                        list.remove(1);

                        // Move current card to last
                        list.remove();
                        list.add(current);
                        found = true;
                        break;
                    }
                }

                if (found)
                    break;
            }
        }

        if (part2) {
            int pos = list.indexOf(1);
            return list.get(pos + 1) * list.get(pos + 2);
        } else {
            int pos = list.indexOf(1);
            StringBuilder s = new StringBuilder();
            for (int i = (pos + 1) % 9; i != pos; i = (i + 1) % 9) {
                char c = (char) (list.get(i) + 48);
                s.append(c);
            }
            return s.toString();
        }
    }

    @Test
    public void testPart() throws Exception {
        // part1
        assertEquals("92658374", solve(false, 10, true));
        assertEquals("67384529", solve(false, 100, true));

        // part2
        assertEquals(149245887792L, solve(true, 100, true));
        // assertEquals("28946753", solve(true));
    }

}
