package day11;

import static org.junit.Assert.*;

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
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.Consumer;

import org.junit.Test;

import utils.SearchAlgorithms;

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
        Hydrogen,
        Lithium,
        Strontium,
        Plutonium,
        Curium,
        Thulium,
        Ruthenium
    }

    enum Type {
        RTG,
        Microchip
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

        /**
         * Return true/false if this component will fry the component specified
         * in the first argument.
         * 
         * @param components
         * @return
         */
        boolean willFry(Component other) {
            boolean fry = (type == Type.RTG && other.type == Type.Microchip
                    && compatibility != other.compatibility);
            // if (fry) {
            // System.out.format("%s fries %s%n", this, other);
            // } else {
            // System.out.format("%s does not fry %s%n", this, other);
            // }
            return fry;
        }

        @Override
        public int hashCode() {
            final int prime = 31;
            int result = 1;
            result = prime * result
                    + ((compatibility == null) ? 0 : compatibility.hashCode());
            result = prime * result + ((type == null) ? 0 : type.hashCode());
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
            if (compatibility != other.compatibility)
                return false;
            if (type != other.type)
                return false;
            return true;
        }

        @Override
        public int compareTo(Component o) {
            int x = compatibility.compareTo(o.compatibility);
            if (x != 0)
                return x;
            else {
                return type.compareTo(o.type);
            }
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

    static class Move {
        int from;
        int to;
        List<Component> components = new ArrayList<>();

        public Move(int from, int to, List<Component> components) {
            super();
            this.from = from;
            this.to = to;
            this.components = components;
        }

        @Override
        public String toString() {
            return String.format("move %s from floor %d to floor %d",
                    components, from, to);
        }

    }

    static class Building {
        Building parent = null;
        Move move = null; // The move used to obtain this state
        Map<Component, Integer> components = new HashMap<>();
        int elevator = 1;
        final int lowestFloor;
        final int highestFloor;

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
            return elevator == other.elevator
                    && components.equals(other.components);
        }

        public int hashCode() {
            return Integer.hashCode(elevator) * components.hashCode();
        }

        @Override
        public String toString() {
            StringBuilder sb = new StringBuilder();

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

        Building applyMove(Move move) {
            Building newBuilding = new Building(this);
            if (newBuilding.elevator != move.from) {
                throw new IllegalArgumentException(
                        "Illegal move: elevator is not in this floor");
            }

            if (move.components.size() > 2) {
                throw new IllegalArgumentException(
                        "Illegal move: elevator cannot move more than 2 components at a time.");
            }

            for (Component c : move.components) {
                newBuilding.components.put(c, move.to);
            }

            newBuilding.elevator = move.to;
            newBuilding.parent = this;
            newBuilding.depth = depth + 1;
            newBuilding.move = move;
            return newBuilding;
        }

        void forAdjacentFloors(int floorNum, Consumer<Integer> fun) {
            int upper = floorNum + 1;
            if (upper <= highestFloor)
                fun.accept(upper);

            int lower = floorNum - 1;
            if (lower >= lowestFloor)
                fun.accept(lower);
        }

        /**
         * Return the valid set of moves.
         * 
         * @return
         */
        List<Move> getMoves() {

            List<Component> movableComponents = new ArrayList<>();

            for (Entry<Component, Integer> e : components.entrySet()) {
                if (!e.getValue().equals(elevator))
                    continue;

                movableComponents.add(e.getKey());
            }

            List<Move> moves = new ArrayList<>();

            for (List<Component> complist : getAllTwoCombinations(
                    movableComponents)) {
                forAdjacentFloors(elevator, (adj) -> {
                    Move move = new Move(elevator, adj, complist);
                    Building b = applyMove(move);
                    if (b.isValid()) {
                        moves.add(move);
                    }
                });
            }

            // System.out.println("Possible moves from\n" + this);
            // moves.stream().forEach(System.out::println);
            return moves;
        }

        private boolean isValid() {
            for (Component c: )
        }

        /**
         * Returns true/false if any of the given components will be fried at
         * the given floor.
         */
        public boolean willAnyComponentBeFriedAt(int floor,
                List<Component> componentsToMove) {

            // Make sure that all the chips at this floor are shielded.
            for (Component tomove : componentsToMove) {
                for (Entry<Component, Integer> atfloor : components
                        .entrySet()) {
                    if (!atfloor.getValue().equals(floor))
                        continue;

                    if (atfloor.getKey().willFry(tomove)) {
                        System.out.format("%s will fry %s at floor %d%n",
                                atfloor.getKey(), tomove, floor);
                        return true;
                    }

                    if (tomove.willFry(atfloor.getKey())) {
                        System.out.format("%s will fry %s at floor %d%n",
                                tomove, atfloor.getKey(), floor);
                        return true;
                    }
                }
            }

            System.out.format("No components of %s will be fried at floor %s%n",
                    componentsToMove, floor);

            return false;

        }

        public boolean isSolution() {
            // System.out.println("Day11.Building.isSolution():\n" + this);
            return components.entrySet().stream()
                    .allMatch(e -> e.getValue().equals(highestFloor));
        }

    }

    @Test
    public void testGetMoves() throws Exception {
        Building building = getTestInput();
        List<Move> moves = building.getMoves();
        moves.stream().forEach(System.out::println);

        // There is only one move we can do in the initial configuration,
        // and that is to move the hydrogen chip to the second floor.
        assertEquals(1, moves.size());
        Move move = moves.get(0);
        assertEquals(
                Arrays.asList(
                        new Component(Substance.Hydrogen, Type.Microchip)),
                move.components);
        assertEquals(1, move.from);
        assertEquals(2, move.to);

    }

    @Test
    public void testFrying() throws Exception {
        Building building = getTestInput();

        // Moving the lithium microchip (in the initial configuration)
        // to floor 2 will fry it, since there is a hydrogen RTG but
        // no litium RTG.
        assertTrue(building.willAnyComponentBeFriedAt(2, Arrays
                .asList(new Component(Substance.Lithium, Type.Microchip))));

        // The Lithium RTG will fry the hydrogen microhip at floor 1.
        assertTrue(building.willAnyComponentBeFriedAt(1,
                Arrays.asList(new Component(Substance.Lithium, Type.RTG))));

        // Moving to the third floor is ok, because there is only a lithium RTG
        // there.
        assertFalse(building.willAnyComponentBeFriedAt(3, Arrays
                .asList(new Component(Substance.Lithium, Type.Microchip))));
    }

    @Test
    public void testApplyMove() throws Exception {
        Building building = getTestInput();
        System.out.println("Initial state:\n" + building);

        List<Move> moves = building.getMoves();
        Move move = moves.get(0);

        System.out.println("Executing " + move);

        Building newBuilding = building.applyMove(move);

        // Check that the elevator has moved
        assertEquals(move.to, newBuilding.elevator);

        // Check that all the components have been moved
        move.components.stream()
                .allMatch(c -> newBuilding.components.get(c).equals(move.to));

        System.out.println("Resulting state:\n" + newBuilding);

        List<Move> moves2 = newBuilding.getMoves();
        System.out.println("Moves available from second state: " + moves2);
    }

    @Test
    public void testSolve1() throws Exception {
        Building building = new Building(1, 4);
        building.elevator = 3;
        building.addComponent(new Component(Substance.Hydrogen, Type.Microchip),
                3);
        building.addComponent(new Component(Substance.Lithium, Type.Microchip),
                3);
        building.addComponent(new Component(Substance.Hydrogen, Type.RTG), 4);
        building.addComponent(new Component(Substance.Lithium, Type.RTG), 4);

        List<Move> moves = building.getMoves();
        moves.stream().forEach(System.out::println);
        System.out.println();
    }

    @Test
    public void testSolve() throws Exception {

        Building building = new Building(1, 4);
        building.elevator = 3;
        building.addComponent(new Component(Substance.Hydrogen, Type.Microchip),
                3);
        building.addComponent(new Component(Substance.Lithium, Type.Microchip),
                3);
        building.addComponent(new Component(Substance.Hydrogen, Type.RTG), 4);
        building.addComponent(new Component(Substance.Lithium, Type.RTG), 4);

        // Building building = getTestInput();
        AtomicInteger counter = new AtomicInteger();
        long start = System.nanoTime();

        Optional<List<Building>> solution = SearchAlgorithms.breadthFirstSearch(
                building, Building::getMoves, Building::applyMove, (b) -> {
                    int x = counter.incrementAndGet();
                    if (x % 1000 == 0) {
                        long elapsed = System.nanoTime() - start;
                        long elapsedPerSolution = elapsed / x;
                        System.out.format(
                                "Number of solutions checked: %d (%d usecs/per solution)%n",
                                x, TimeUnit.NANOSECONDS
                                        .toMicros(elapsedPerSolution));

                    }

                    System.out.println("\n===== Start state:\n" + building);
                    List<Move> moves = new ArrayList<>();
                    Building step = b;
                    while (step != null) {
                        if (step.move != null)
                            moves.add(step.move);
                        step = step.parent;
                    }
                    Collections.reverse(moves);
                    moves.stream().forEach(System.out::println);
                    System.out.println("====== End state:\n" + b);
                    System.out.println("===================================");
                    // There is supposed to be a solution with 11 steps.
                    if (b.depth > 12)
                        throw new AssertionError(
                                "Maximum expected depth exceeded");

                    return b.isSolution();
                }, (b) -> b.parent);

        assertTrue(solution.isPresent());

        solution.get().stream().forEach(System.out::println);
    }
}
