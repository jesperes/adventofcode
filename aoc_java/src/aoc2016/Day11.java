package aoc2016;

import static org.junit.Assert.assertEquals;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Deque;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.junit.Test;

import com.google.common.collect.Lists;
import com.google.common.collect.Streams;

import aoc2016.Day11.State;
import common2.AocPuzzleInfo;
import common2.AocResult;
import common2.IAocIntPuzzle;

public class Day11 implements IAocIntPuzzle<State> {

    record Substance(long id, int genFloor, int chipFloor) {
    }

    record State(List<Integer> components, int elevator) {

        public String toString() {
            StringBuilder b = new StringBuilder();

            for (int floor = 4; floor >= 1; floor--) {
                b.append(String.format("F%d ", floor));

                if (elevator == floor) {
                    b.append("E ");
                } else {
                    b.append(". ");
                }

                for (int i = 0; i < components.size(); i++) {
                    char type = (i % 2 == 0) ? 'G' : 'M';
                    int id = Math.floorDiv(i, 2);
                    char subst = (char) (id + 'A');
                    if (components.get(i) == floor) {
                        b.append(String.format("%c%c ", subst, type));
                    } else {
                        b.append(".  ");
                    }
                }
                b.append("\n");
            }
            return b.toString();
        }
    }

    // Unpacks this state into a set of substances
    List<Substance> unpack(State state) {
        return Streams
                .mapWithIndex(Lists.partition(state.components, 2).stream(),
                        (pair, id) -> new Substance(id, pair.get(0).intValue(),
                                pair.get(1)))
                .collect(Collectors.toUnmodifiableList());
    }

    static void pack(State oldState, List<State> states,
            List<Substance> substances, int elevator, Substance... replace) {

        List<Substance> list = new ArrayList<>();
        for (Substance s : substances) {
            boolean replaced = false;
            for (Substance r : replace) {
                if (s.id == r.id) {
                    list.add(r);
                    replaced = true;
                }
            }

            if (!replaced)
                list.add(s);
        }

        // Check that all substances are either shielded or on a floor
        // without another generator on.
        for (Substance a : list) {
            if (a.chipFloor == a.genFloor)
                // chip(a) is shielded by gen(a)
                continue;

            for (Substance other : list) {
                if (a.id != other.id) {
                    if (a.chipFloor == other.genFloor) {
                        return;
                    }
                }
            }
        }

        // At this point, we know that the state is valid, and we can
        // add it to the list.

        State state = new State(list.stream().flatMap(subst -> {
            for (Substance r : replace) {
                if (r.id == subst.id) {
                    return Stream.of(r.genFloor, r.chipFloor);
                }
            }
            return Stream.of(subst.genFloor, subst.chipFloor);
        }).collect(Collectors.toUnmodifiableList()), elevator);

//        System.out.println("-------------------------------------------");
//        System.out.println("Old state:");
//        System.out.println(oldState);
//        System.out.println(" -> ");
//        System.out.println();
//        System.out.println(state);
//        System.out.println("-------------------------------------------");
//        System.out.println();
        states.add(state);
    }

    boolean isChipShielded(Substance subst) {
        return subst.genFloor == subst.chipFloor;
    }

    boolean isTargetState(State state) {
        for (int n : state.components) {
            if (n != 4)
                return false;
        }
        return true;
    }

    /**
     * Returns a collection of possible next states.
     */
    List<State> getNextStates(State state) {
        List<State> states = new ArrayList<>();
        List<Substance> substances = unpack(state);
        int elevator = state.elevator;
        int[] destFloors = new int[] { elevator + 1, elevator - 1 };

        int min = Integer.MAX_VALUE;
        for (var n : state.components) {
            min = Math.min(n, min);
        }

        for (int destFloor : destFloors) {
            if (destFloor < min || destFloor > 4)
                continue;

            for (Substance a : substances) {
                for (Substance b : substances) {
                    if (a.id == b.id) {
                        if (a.chipFloor == elevator && a.genFloor == elevator) {
                            // Move chip and gen of same kind
                            pack(state, states, substances, destFloor,
                                    new Substance(a.id, destFloor, destFloor));
                        }
                    } else {
                        // Move either two chips or two generators.
                        // (We can never move a chip + generator of different
                        // kinds; the generator will fry the chip.)
                        if (a.chipFloor == elevator
                                && b.chipFloor == elevator) {
                            pack(state, states, substances, destFloor,
                                    new Substance(a.id, a.genFloor, destFloor),
                                    new Substance(b.id, b.genFloor, destFloor));
                        }
                        if (a.genFloor == elevator && b.genFloor == elevator) {
                            pack(state, states, substances, destFloor,
                                    new Substance(a.id, destFloor, a.chipFloor),
                                    new Substance(b.id, destFloor,
                                            b.chipFloor));
                        }
                    }
                }

                // Move just chip(a)
                if (a.chipFloor == elevator) {
                    pack(state, states, substances, destFloor,
                            new Substance(a.id, a.genFloor, destFloor));
                }

                // Move just gen(a)
                if (a.genFloor == elevator) {
                    pack(state, states, substances, destFloor,
                            new Substance(a.id, destFloor, a.chipFloor));
                }
            }
        }

        return states;
    }

    @Override
    public AocPuzzleInfo getInfo() {
        return new AocPuzzleInfo(2016, 11,
                "Radioisotope Thermoelectric Generators", false);
    }

    @Override
    public AocResult<Integer, Integer> getExpected() {
        return AocResult.of(37, 61);
    }

    @Override
    public State parse(Optional<File> file) throws IOException {
        return part1input();
    }

    @Override
    public Integer part1(State input) {
        Deque<State> queue = new LinkedList<>();
        Set<State> seen = new HashSet<>();
        Map<State, Integer> depthMap = new HashMap<>();
        int count = 0;

        queue.add(input);
        while (!queue.isEmpty()) {
            var s = queue.removeFirst();
            var depth = depthMap.getOrDefault(s, 0);

            count++;

            if (count % 100000 == 0) {
                System.out.println("Count: " + count);
                System.out.println("Depth: " + depth);
            }

            if (isTargetState(s)) {
                System.out.println("Done!");
                System.out.println("Depth: " + depth);
                System.out.println("States checked: " + count);
                return depth;
            }

            for (State n : getNextStates(s)) {
                if (seen.contains(n)) {
                    continue;
                } else {
//                    System.out.println("New state seen:");
//                    System.out.println(n);
                    queue.add(n);
                    seen.add(n);
                    depthMap.put(n, depth + 1);
                }
            }
        }
        throw new RuntimeException();
    }

    @Override
    public Integer part2(State input) {
        return 0;
    }

    /*
     * Tests
     */

    @Test
    public void testEquals() throws Exception {
        assertEquals(parse(null), parse(null));
    }

//    @Test
//    public void testPackState() throws Exception {
//        {
//            var state = ex1State();
//            assertEquals(state, pack(unpack(state), state.elevator));
//        }
//        {
//            var state = part1input();
//            assertEquals(state, pack(unpack(state), state.elevator));
//        }
//        {
//            var state = part2input();
//            assertEquals(state, pack(unpack(state), state.elevator));
//        }
//    }

    @Test
    public void testPart1() throws Exception {
        part1(ex1State());
        part1(part1input());
        part1(part2input());
    }

    @Test
    public void testToString() throws Exception {
        System.out.println(ex1State());
    }

    private State part1input() {
        // The first floor contains a strontium generator, a
        // strontium-compatible microchip, a plutonium generator, and a
        // plutonium-compatible microchip.
        // The second floor contains a thulium generator, a ruthenium generator,
        // a ruthenium-compatible microchip, a curium generator, and a
        // curium-compatible microchip.
        // The third floor contains a thulium-compatible microchip.
        // The fourth floor contains nothing relevant.
        return new State(//
                List.of(1, 1, // strontium
                        1, 1, // plutonium
                        2, 2, // ruthenium
                        2, 2, // curium
                        2, 3 // thulium
                ), 1);
    }

    private State part2input() {
        // Like part1, but elerium + dilithium on first floor.
        return new State(//
                List.of(1, 1, // strontium
                        1, 1, // plutonium
                        1, 1, // elerium
                        1, 1, // dilithium
                        2, 2, // ruthenium
                        2, 2, // curium
                        2, 3 // thulium
                ), 1);
    }

    private State ex1State() {
        // The first floor contains a hydrogen-compatible microchip and a
        // lithium-compatible microchip.
        // The second floor contains a hydrogen generator.
        // The third floor contains a lithium generator.
        // The fourth floor contains nothing relevant.
        return new State(List.of(//
                2, 1, // hydrogen
                3, 1 // lithium
        ), 1);
    }

}
