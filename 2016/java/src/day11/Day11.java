package day11;

import static org.junit.Assert.*;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.function.Consumer;
import java.util.stream.Collectors;

import org.junit.Test;

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
        List<List<T>> list = new ArrayList<>();

        for (T elem : collection) {
            list.add(Arrays.asList(elem));
        }

        for (T a : collection) {
            for (T b : collection) {
                if (a.compareTo(b) < 0) {
                    list.add(Arrays.asList(a, b));
                }
            }
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

        /**
         * Return true/false if this component will fry the component specified
         * in the first argument.
         * 
         * @param components
         * @return
         */
        boolean willFry(Component other) {
            return type == Type.RTG && other.type == Type.Microchip
                    && compatibility != other.compatibility;
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
        Map<Component, Integer> components = new HashMap<>();
        int elevator = 1;
        final int lowestFloor;
        final int highestFloor;

        public Building(int low, int high) {
            this.lowestFloor = low;
            this.highestFloor = high;
        }

        void addComponent(Component c, int floorNum) {
            components.put(c, floorNum);
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
            List<Component> movableComponents = components.entrySet().stream()
                    .filter(e -> e.getValue() == elevator).map(e -> e.getKey())
                    .collect(Collectors.toList());

            List<Move> moves = new ArrayList<>();

            getAllTwoCombinations(movableComponents).stream().forEach(c -> {
                forAdjacentFloors(elevator, (adj) -> {
                    if (!willAnyComponentBeFriedAt(adj, c)) {
                        moves.add(new Move(elevator, adj, c));
                    }
                });
            });

            return moves;
        }

        /**
         * Returns true/false if any of the given components will be fried at
         * the given floor.
         */
        public boolean willAnyComponentBeFriedAt(Integer floor,
                List<Component> componentsToMove) {

            Set<Substance> rtgs = components.entrySet().stream().filter(
                    e -> e.getValue() == floor && e.getKey().type == Type.RTG)
                    .map(e -> e.getKey().compatibility)
                    .collect(Collectors.toSet());

            Set<Substance> chipsToMove = componentsToMove.stream()
                    .filter(c -> c.type == Type.Microchip)
                    .map(c -> c.compatibility).collect(Collectors.toSet());

            // Make sure that all the chips at this floor are shielded.
            for (Substance chip : chipsToMove) {
                boolean isShielded = false;
                boolean needsShielding = false;

                for (Substance rtg : rtgs) {
                    if (rtg.equals(chip)) {
                        // ok, chip is shielded from this rtg
                        isShielded = true;
                    } else {
                        // not ok, chip is not shielded from other rtg.
                        needsShielding = true;
                    }
                }

                if (needsShielding && !isShielded)
                    return true;
            }

            return false;
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

        // Moving to the third floor is ok, because there is only a lithium RTG
        // there.
        assertFalse(building.willAnyComponentBeFriedAt(3, Arrays
                .asList(new Component(Substance.Lithium, Type.Microchip))));
    }
}
