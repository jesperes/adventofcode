package day24;

import static org.junit.Assert.*;

import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.junit.Test;

import utils.Utils;

public class Day24 {

    LinkedList<Component> components = new LinkedList<>(
            Stream.of("0/2", "2/2", "2/3", "3/4", "3/5", "0/1", "10/1", "9/10")
                    .map(s -> new Component(s)).collect(Collectors.toList()));

    static class Component {
        final int p_in;
        final int p_out;

        public Component(String str) {
            String[] elems = str.split("/");
            this.p_in = Integer.valueOf(elems[0]);
            this.p_out = Integer.valueOf(elems[1]);
        }

        public Component(int p_in, int p_out) {
            this.p_in = p_in;
            this.p_out = p_out;
        }

        /**
         * Flip this component if necessary to match the given number of
         * in-pins. Returns an empty optional if the component does not match.
         */
        public Optional<Component> maybeFlip(int in) {
            if (p_in == in)
                return Optional.of(this);
            else if (p_out == in)
                return Optional.of(new Component(p_out, p_in));
            else
                return Optional.empty();
        }

        @Override
        public String toString() {
            return String.format("%d/%d", p_in, p_out);
        }

        @Override
        public int hashCode() {
            final int prime = 31;
            int result = 1;
            result = prime * result + p_in;
            result = prime * result + p_out;
            return result;
        }

        @Override
        public boolean equals(Object obj) {
            if (this == obj)
                return true;
            if (obj == null)
                return false;
            if (getClass() != obj.getClass())
                return false;
            Component other = (Component) obj;
            if (p_in != other.p_in)
                return false;
            if (p_out != other.p_out)
                return false;
            return true;
        }

    }

    static int strength(List<Component> bridge) {
        int s = bridge.stream().mapToInt(c -> c.p_in + c.p_out).sum();
        // System.out.format("Length %s == %d%n", bridge, s);
        return s;
    }

    @SafeVarargs
    static final <T> LinkedList<T> linkedListOf(T... elems) {
        LinkedList<T> list = new LinkedList<T>();
        for (T e : elems) {
            list.add(e);
        }
        return list;
    }

    /**
     * Compute the strongest bridge which can be formed by taking 'first' and
     * appending elements from 'components'.
     * 
     * @param first
     * @param components
     * @return
     */
    List<LinkedList<Component>> getAllBridges(int first,
            LinkedList<Component> components) {

        List<LinkedList<Component>> list = new ArrayList<>();

        // Find all components matching the digit we are to start with.
        for (Component comp : components) {
            comp.maybeFlip(first).ifPresent(match -> {

                /*
                 * There is always a sub-list containing just the matching
                 * elements (in case there are no more matching elements in the
                 * the rest of the list.
                 */
                list.add(linkedListOf(match));

                /*
                 * Construct all sub-bridges from the list of components minus
                 * the one we've already used up in this step.
                 */
                LinkedList<Component> sublist = new LinkedList<>();
                components.forEach(c -> {
                    if (!c.equals(comp))
                        sublist.add(c);
                });

                List<LinkedList<Component>> allSubBridges = getAllBridges(
                        match.p_out, sublist);

                /*
                 * Pre-pend 'comp' (the component we have used in this step) to
                 * all of the output lists.
                 */
                for (LinkedList<Component> o : allSubBridges) {
                    o.add(0, match); // 'match' is comp, but flipped to match
                    list.add(o);
                }
            });
        }

        return list;
    }

    public List<Component> getStrongestBridge(
            List<LinkedList<Component>> allBridges) {

        int maxStrength = Integer.MIN_VALUE;
        List<Component> strongest = null;

        for (LinkedList<Component> l : allBridges) {
            int strength = strength(l);
            if (strength > maxStrength) {
                strongest = l;
                maxStrength = strength;
            }
        }

        return strongest;
    }

    public List<Component> getLongestBridge(
            List<LinkedList<Component>> allBridges) {

        int maxLength = Integer.MIN_VALUE;
        int currentStrength = 0; // strength of currently longest bridge
        List<Component> longest = null;

        for (LinkedList<Component> l : allBridges) {
            int length = l.size();
            if (length > maxLength) {
                longest = l;
                maxLength = length;
                currentStrength = strength(l);
            } else if (length == maxLength) {
                int s = strength(l);
                if (s > currentStrength) {
                    longest = l;
                    maxLength = length;
                    currentStrength = s;
                }
            }
        }

        return longest;
    }

    @Test
    public void testMaybeFlip() throws Exception {
        Component p1 = new Component(0, 1);
        assertEquals(Optional.of(new Component(1, 0)), p1.maybeFlip(1));
        assertEquals(Optional.of(new Component(0, 1)), p1.maybeFlip(0));
    }

    @Test
    public void miniTestCase() throws Exception {
        List<LinkedList<Component>> list = getAllBridges(0,
                linkedListOf(new Component(0, 1), new Component(0, 3),
                        new Component(2, 1), new Component(1, 3)));
        assertEquals(8, list.size());
    }

    @Test
    public void bridgeStrength() throws Exception {
        assertEquals(7, strength(linkedListOf(new Component(0, 1),
                new Component(0, 3), new Component(2, 1))));
    }

    @Test
    public void shortTestCase() throws Exception {
        List<LinkedList<Component>> allBridges = getAllBridges(0, components);
        assertEquals(11, allBridges.size());
        assertEquals(31, strength(getStrongestBridge(allBridges)));
        assertEquals(19, strength(getLongestBridge(allBridges)));
    }

    @Test
    public void fullTestCase() throws Exception {
        LinkedList<Component> list = linkedListOf();
        Utils.readFileFromClassPathAsLines(getClass(), "day24/input.txt")
                .map(s -> new Component(s)).forEach(list::add);

        List<LinkedList<Component>> allBridges = getAllBridges(0, list);

        List<Component> strongest = getStrongestBridge(allBridges);
        List<Component> longest = getLongestBridge(allBridges);

        System.out.println("Strongest bridge has length: " + strongest.size());
        System.out.println(
                "Strongest bridge has strength " + strength(strongest));
        System.out.println("Strongest bridge: " + strongest);

        System.out.println("Longest bridge has length " + longest.size());
        System.out.println("Longest bridge has strength " + strength(longest));
        System.out.println("Longest bridge: " + longest);

        assertEquals(2006, strength(strongest));
        assertEquals(1994, strength(longest));
    }
}
