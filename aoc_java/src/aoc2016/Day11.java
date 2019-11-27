package aoc2016;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Optional;
import java.util.SortedSet;
import java.util.TreeSet;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.stream.Collectors;

import org.junit.Ignore;
import org.junit.Test;

import common.SearchAlgorithms;

/**
 * Rules:
 * 
 * -
 * 
 * @author jesperes
 *
 */
public class Day11 {

    enum Substance {
        Hydrogen, Lithium, Strontium, Plutonium, Curium, Thulium, Ruthenium
    }

    enum Type {
        RTG, Microchip
    }

    static <T extends Comparable<T>> List<List<T>> getAllTwoCombinations(
            Collection<T> collection) {
        if (collection.isEmpty())
            return Collections.emptyList();

        List<List<T>> list = new ArrayList<>();

        for (T a : collection) {
            for (T b : collection) {
                if (a.compareTo(b) < 0) {
                    list.add(Arrays.asList(a, b));
                }
            }
        }

        for (T elem : collection) {
            list.add(Arrays.asList(elem));
        }

        return list;
    }

    static class Component implements Comparable<Component> {
        final Substance compatibility;
        final Type type;

        public static Component of(Substance compat, Type type) {
            return new Component(compat, type);
        }

        @Override
        public String toString() {
            return String.format("%s %s", compatibility, type);
        }

        public Component(Substance compatibility, Type type) {
            this.compatibility = compatibility;
            this.type = type;
        }

        public String shortName() {
            return String.format("%s%s",
                    Character.toUpperCase(compatibility.toString().charAt(0)),
                    type == Type.RTG ? "G" : "M");
        }

        @Override
        public int hashCode() {
            return compatibility.hashCode() * type.hashCode();
        }

        @Override
        public boolean equals(Object obj) {
            Component other = (Component) obj;
            return other.compatibility == compatibility && other.type == type;
        }

        @Override
        public int compareTo(Component o) {
            return shortName().compareTo(o.shortName());
        }
    }

    static Building getTestInput() {
        Building building = new Building(1, 4);

        building.addComponent(new Component(Substance.Hydrogen, Type.Microchip),
                1);
        building.addComponent(new Component(Substance.Lithium, Type.Microchip),
                1);
        building.addComponent(new Component(Substance.Hydrogen, Type.RTG), 2);
        building.addComponent(new Component(Substance.Lithium, Type.RTG), 3);
        return building;
    }

    static class Building {

        Building parent = null;
        Map<Component, Integer> components = new HashMap<>();
        int elevator = 1;
        final int lowestFloor;
        final int highestFloor;

        static AtomicInteger counter = new AtomicInteger();
        int id = counter.incrementAndGet();
        int depth = 0; // track search depth

        public Building(int low, int high) {
            this.lowestFloor = low;
            this.highestFloor = high;
        }

        public Building(Building other) {
            this.lowestFloor = other.lowestFloor;
            this.highestFloor = other.highestFloor;
            this.elevator = other.elevator;
            this.components = new HashMap<>(other.components);
        }

        @Override
        public boolean equals(Object obj) {
            Building other = (Building) obj;
            return components.equals(other.components);
        }

        public int hashCode() {
            return Integer.hashCode(elevator) * components.hashCode();
        }

        @Override
        public String toString() {
            StringBuilder sb = new StringBuilder();
            sb.append(String.format(
                    "\nSolution (id %d) at depth %d (parent solution id %s)%n",
                    id, depth, parent != null ? parent.id : "<none>"));

            SortedSet<Component> set = new TreeSet<>(components.keySet());

            for (int i = highestFloor; i >= lowestFloor; i--) {
                sb.append(String.format("F%d %-2s", i,
                        (i == elevator) ? "E" : "."));

                for (Component c : set) {
                    sb.append(String.format("%-3s",
                            components.get(c).equals(i) ? c.shortName() : "."));
                }

                sb.append("\n");
            }
            return sb.toString();
        }

        void addComponent(Component c, int floorNum) {
            components.put(c, floorNum);
        }

        int[] floorDeltas = new int[] { 1, -1 };

        /**
         * Return all possible buildings which can be reached from this point.
         * 
         * @return
         */
        public List<Building> getChildren() {

            List<Building> children = new ArrayList<>();

            // Collect all the components on the same floor as the elevator.
            List<Component> movableComponents = components.keySet().stream()
                    .filter(c -> components.get(c).intValue() == elevator)
                    .collect(Collectors.toList());

            List<List<Component>> combinations = getAllTwoCombinations(
                    movableComponents);
            for (List<Component> complist : combinations) {

                for (int delta : floorDeltas) {
                    int dest = elevator + delta;
                    if (dest <= highestFloor && dest >= lowestFloor) {
                        // Move components to a higher floor.
                        Building newstate = new Building(this);
                        newstate.elevator = dest;
                        newstate.parent = this;
                        newstate.depth = depth + 1;

                        for (Component componentToMove : complist) {
                            newstate.components.put(componentToMove, dest);
                        }

                        // Check if the resulting configuration is valid.
                        if (newstate.isValid()) {
                            children.add(newstate);
                        }
                    }
                }
            }

            // System.out.println("== Children list ==");
            // System.out.println("Parent state: " + this);
            // children.stream().forEach(System.out::println);
            // System.out.println("== End children list ==");

            return children;
        }

        public boolean isValid() {
            for (Entry<Component, Integer> e : components.entrySet()) {

                // For all RTGs
                if (e.getKey().type == Type.RTG) {
                    for (Entry<Component, Integer> e1 : components.entrySet()) {

                        if (
                        // If it is a microchip...
                        e1.getKey().type == Type.Microchip

                                // on the same floor as the RTG...
                                && e1.getValue().equals(e.getValue())

                                // and is not compatible with the RTG...
                                && e1.getKey().compatibility != e
                                        .getKey().compatibility

                                // and is not shielded by its own RTG...
                                && components.get(Component.of(
                                        e1.getKey().compatibility, Type.RTG))
                                        .intValue() != e1.getValue()
                                                .intValue()) {

                            // then it is fried, and the state is invalid.
                            return false;
                        }
                    }
                }
            }

            return true;
        }

        public boolean isSolution() {
            return components.entrySet().stream()
                    .allMatch(e -> e.getValue().equals(highestFloor));
        }
    }

    class BuildingSearchAdapter
            implements SearchAlgorithms.BreadthFirstSearch<Building> {

        @Override
        public List<Building> getChildren(Building node) {
            return node.getChildren();
        }

        @Override
        public boolean isSolution(Building node) {
            System.out.println("Checking solution: " + node);
            if (!node.isValid())
                throw new AssertionError(
                        "Suggested solution is not valid: " + node);
            return node.isSolution();
        }

        @Override
        public Building getParent(Building node) {
            return node.parent;
        }

    }

    @Test
    public void testGetChildren() throws Exception {
        Building building = getTestInput();
        Building expected = new Building(building);
        expected.components
                .put(Component.of(Substance.Hydrogen, Type.Microchip), 2);
        assertTrue(expected.isValid());
        List<Building> children = building.getChildren();
        assertEquals(1, children.size());
        assertEquals(expected, children.get(0));
    }

    @Test
    public void testGetChildren2() throws Exception {
        // This state is one move away from a solution.
        Building building = getTestInput();
        building.elevator = 3;
        building.components
                .put(Component.of(Substance.Hydrogen, Type.Microchip), 3);
        building.components.put(Component.of(Substance.Lithium, Type.Microchip),
                3);
        building.components.put(Component.of(Substance.Hydrogen, Type.RTG), 4);
        building.components.put(Component.of(Substance.Lithium, Type.RTG), 4);

        Building solution = new Building(building);
        solution.components
                .put(Component.of(Substance.Hydrogen, Type.Microchip), 4);
        solution.components.put(Component.of(Substance.Lithium, Type.Microchip),
                4);
        solution.components.put(Component.of(Substance.Hydrogen, Type.RTG), 4);
        solution.components.put(Component.of(Substance.Lithium, Type.RTG), 4);
        assertTrue(solution.isValid());
        assertTrue(solution.isSolution());

        List<Building> children = building.getChildren();

        // Check that the solution state is among the children.
        assertTrue(children.stream().anyMatch(b -> b.equals(solution)));
    }

    @Test
    public void testIsValid() throws Exception {
        {
            Building building = getTestInput();
            assertTrue(building.isValid());
        }

        {
            Building building = new Building(1, 4);
            building.addComponent(
                    new Component(Substance.Hydrogen, Type.Microchip), 3);
            building.addComponent(
                    new Component(Substance.Lithium, Type.Microchip), 3);
            building.addComponent(new Component(Substance.Hydrogen, Type.RTG),
                    4);
            building.addComponent(new Component(Substance.Lithium, Type.RTG),
                    4);
            assertTrue(building.isValid());
        }

        {
            Building building = new Building(1, 4);
            building.addComponent(
                    new Component(Substance.Hydrogen, Type.Microchip), 4);
            building.addComponent(
                    new Component(Substance.Lithium, Type.Microchip), 3);
            building.addComponent(new Component(Substance.Hydrogen, Type.RTG),
                    3);
            building.addComponent(new Component(Substance.Lithium, Type.RTG),
                    4);
            assertFalse(building.isValid());
        }
    }

    @Test
    public void testIsValid2() throws Exception {
        Building building = new Building(1, 4);
        building.addComponent(new Component(Substance.Hydrogen, Type.Microchip),
                3);
        building.addComponent(new Component(Substance.Lithium, Type.Microchip),
                4);
        building.addComponent(new Component(Substance.Hydrogen, Type.RTG), 4);
        building.addComponent(new Component(Substance.Lithium, Type.RTG), 4);
        assertTrue(building.isValid());
    }

    // @Test
    // public void testStep() throws Exception {
    // Building building = getTestInput();
    // building.elevator = 3;
    // building.components
    // .put(Component.of(Substance.Hydrogen, Type.Microchip), 3);
    // building.components.put(Component.of(Substance.Lithium, Type.Microchip),
    // 3);
    // building.components.put(Component.of(Substance.Hydrogen, Type.RTG), 3);
    // building.components.put(Component.of(Substance.Lithium, Type.RTG), 3);
    //
    // System.out.println(building);
    // System.out.println("Possible steps:");
    // building.getChildren().stream().forEach(System.out::println);
    // }

    @Test
    @Ignore
    public void testSolve1() throws Exception {
        Building building = getTestInput();
        building.elevator = 3;
        building.components
                .put(Component.of(Substance.Hydrogen, Type.Microchip), 3);
        building.components.put(Component.of(Substance.Lithium, Type.Microchip),
                4);
        building.components.put(Component.of(Substance.Hydrogen, Type.RTG), 4);
        building.components.put(Component.of(Substance.Lithium, Type.RTG), 4);

        Optional<List<Building>> solution = SearchAlgorithms
                .breadthFirstSearch(building, new BuildingSearchAdapter());

        assertTrue(solution.isPresent());

        assertTrue(solution.get().stream().allMatch(c -> c.isValid()));

        System.out.println("Final solution steps:");
        solution.get().stream().forEach(System.out::println);
        System.out.println("Steps: " + solution.get().size());
    }
}
