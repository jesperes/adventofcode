package aoc2017;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;

import java.io.IOException;
import java.util.Map;
import java.util.Optional;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.junit.Test;

import common.AocPuzzle;

public class Day13 extends AocPuzzle {

    public Day13() {
        super(2017, 13);
    }

    private Map<Integer, Firewall> getShortInput() {
        return Stream.of(
        // @formatter:off
                "0: 3",
                "1: 2",
                "4: 4",
                "6: 4")
                // @formatter:on
                .map(s -> new Firewall(s))
                .collect(Collectors.toMap(fw -> fw.depth, Function.identity()));
    }

    private Map<Integer, Firewall> getLongInput() throws IOException {
        try (Stream<String> lines = getInputAsStream()) {
            return lines.map(s -> new Firewall(s)).collect(
                    Collectors.toMap(fw -> fw.depth, Function.identity()));
        }
    }

    class Firewall {
        public final int depth;
        public final int range;

        public Firewall(String line) {
            String[] elems = line.split(":");
            this.depth = Integer.valueOf(elems[0].trim());
            this.range = Integer.valueOf(elems[1].trim());
        }

        public boolean isAtTop(int time) {
            /*
             * Is the scanner at top? It will be at the top every ((range * 2) -
             * 2) steps.
             */
            return time % ((range * 2) - 2) == 0;
        }

        public int getSeverity() {
            return depth * range;
        }
    }

    private int getMaxDepth(Map<Integer, Firewall> firewalls) {
        return firewalls.keySet().stream().mapToInt(Integer::intValue).max()
                .getAsInt();
    }

    private int getMaxRange(Map<Integer, Firewall> firewalls) {
        return firewalls.values().stream().map(fw -> fw.range)
                .mapToInt(Integer::intValue).max().getAsInt();
    }

    public static void printFirewalls(Map<Integer, Firewall> firewalls,
            int maxDepth, int maxRange, int time, int currentDepth) {

        System.out.println("Firewall configuration at time == " + time);
        for (int depth = 0; depth <= maxDepth; depth++) {
            System.out.format("%2d  ", depth);
        }

        System.out.println();

        for (int range = 0; range < maxRange; range++) {
            for (int depth = 0; depth <= maxDepth; depth++) {
                if (firewalls.containsKey(depth)) {
                    Firewall fw = firewalls.get(depth);
                    if (range >= fw.range) {
                        System.out.print("    ");
                        continue;
                    }

                    char left;
                    char right;
                    if (currentDepth == depth && range == 0) {
                        left = '(';
                        right = ')';
                    } else {
                        left = '[';
                        right = ']';
                    }

                    if (range == 0) {
                        if (fw.isAtTop(time)) {
                            System.out.format("%cS%c ", left, right);
                        } else {
                            System.out.format("%c %c ", left, right);
                        }
                    } else {
                        System.out.format("%c?%c ", left, right);
                    }

                } else {
                    if (currentDepth == depth && range == 0)
                        System.out.print("(.) ");
                    else if (range == 0)
                        System.out.print("... ");
                    else
                        System.out.print("    ");
                    continue;
                }
            }

            System.out.println();
        }

    }

    /**
     * Compute the severity of any collisions with firewall scanners. Note that
     * we do not need to "simulate" the firewalls, we can simply compute where
     * the scanners are expected to be.
     *
     * @param firewalls      The firewalls. The index here is the depth of the
     *                       firewalls, so if there are no firewall at a certain
     *                       index, the array has a null element.
     * @param maxDepth       The maximum depth of the firewalls
     * @param maxRange       The maximum range of any firewall.
     * @param delay          The delay before starting to travel through the
     *                       firewall.
     * @param onlyCollisions If true, only collisions will be reported, and not
     *                       the severity. This allows the function to return
     *                       immediately when a collision is detected.
     * @return An optional integer. If present, this is the total severity of
     *         all collisions. If onlyCollisions is true, the total severity (in
     *         case of a collision) is 1. If not present, no collisions were
     *         found.
     */
    private Optional<Integer> computeSeverity(Map<Integer, Firewall> firewalls,
            int maxDepth, int maxRange, int delay, boolean onlyCollisions) {

        int time = delay;
        int severity = 0;
        int collisions = 0;

        for (int i = 0; i <= maxDepth; i++) {

            // printFirewalls(firewalls, maxDepth, maxRange, time, i);

            if (firewalls.containsKey(i)) {
                Firewall fw = firewalls.get(i);
                if (fw.isAtTop(time)) {
                    collisions++;
                    severity += fw.getSeverity();
                    if (onlyCollisions) {
                        return Optional.of(1);
                    }
                }
            }

            // advance time
            time++;
        }

        if (collisions == 0)
            return Optional.empty();
        else
            return Optional.of(severity);
    }

    int computeDelayUntilNotCaught(Map<Integer, Firewall> firewalls,
            int maxDepth, int maxRange) {
        for (int delay = 0;; delay++) {
            if (!computeSeverity(firewalls, maxDepth, maxRange, delay, true)
                    .isPresent()) {
                return delay;
            }
        }
    }

    @Test
    public void testShort() throws Exception {
        Map<Integer, Firewall> firewalls = getShortInput();
        int maxDepth = getMaxDepth(firewalls);
        int maxRange = getMaxRange(firewalls);

        assertEquals(24,
                computeSeverity(firewalls, maxDepth, maxRange, 0, false)
                        .orElse(0).intValue());
    }

    @Test
    public void testShort_Part2() throws Exception {
        Map<Integer, Firewall> firewalls = getShortInput();
        int maxDepth = getMaxDepth(firewalls);
        int maxRange = getMaxRange(firewalls);

        assertFalse(computeSeverity(firewalls, maxDepth, maxRange, 10, false)
                .isPresent());
        assertEquals(10,
                computeDelayUntilNotCaught(firewalls, maxDepth, maxRange));
        assertEquals(24,
                computeSeverity(firewalls, maxDepth, maxRange, 0, false)
                        .orElse(-1).intValue());
    }

    @Test
    public void testFull() throws Exception {
        Map<Integer, Firewall> firewalls = getLongInput();
        int maxDepth = getMaxDepth(firewalls);
        int maxRange = getMaxRange(firewalls);
        int part1 = computeSeverity(firewalls, maxDepth, maxRange, 0, false)
                .orElse(0).intValue();
        assertEquals(788, part1);
    }

    @Test
    public void testFull_Part2() throws Exception {
        Map<Integer, Firewall> firewalls = getLongInput();
        int maxDepth = getMaxDepth(firewalls);
        int maxRange = getMaxRange(firewalls);
        int part2 = computeDelayUntilNotCaught(firewalls, maxDepth, maxRange);
        assertEquals(3905748, part2);
    }
}
