package aoc2016;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.Deque;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;

import com.google.common.collect.ComparisonChain;

import aoc2016.Day11.State;
import common2.AocBaseRunner;
import common2.AocPuzzleInfo;
import common2.AocResult;
import common2.IAocIntPuzzle;

/**
 * This is the infamous "Radioisotope Thermoelectric Generators" puzzle from
 * 2016 day 11.
 * 
 * Representation is really important here. Each substance is interchangeable
 * with another as long as their components (microchip and generator) are on the
 * same floors. So we represent a substance (e.g. "hydrogen") as a pair of
 * "genFloor" and "chipFloor" (but not the substance name), and the entire state
 * (i.e. the building) as a (sorted) list of such substances + the elevator
 * floor. This means that symmetric states will be considered equal, and we can
 * exclude them in the search.
 * 
 * Part 2 is constructed such that there is an explosion of equivalent states,
 * so without the ability to exclude equivalent states, part 2 takes forever to
 * run.
 */
public class Day11 implements IAocIntPuzzle<State> {

    class Substance {
        int genFloor;
        int chipFloor;

        public Substance(int genFloor, int chipFloor) {
            this.genFloor = genFloor;
            this.chipFloor = chipFloor;
        }

        @Override
        public int hashCode() {
            return Objects.hash(genFloor, chipFloor);
        }

        @Override
        public boolean equals(Object obj) {
            Substance o = (Substance) obj;
            return chipFloor == o.chipFloor && genFloor == o.genFloor;
        }

        @Override
        public String toString() {
            return String.format("(%d,%d)", genFloor, chipFloor);
        }
    }

    record State(List<Substance> substances, int elevator) {
    }

    boolean isTargetState(State state) {
        for (var subst : state.substances) {
            if (subst.chipFloor != 4 || subst.genFloor != 4) {
                return false;
            }
        }
        return true;
    }

    /**
     * Make a copy of the given state for moving to the given floor. The list of
     * substances will typically be altered afterwards.
     */
    State copy(State old, int destFloor) {
        List<Substance> list = new ArrayList<>();
        for (Substance s : old.substances) {
            list.add(new Substance(s.genFloor, s.chipFloor));
        }
        return new State(list, destFloor);
    }

    /**
     * Checks if a state is valid, and adds it to the given set if so. It will
     * also, if the state is valid, sort the substances so that equivalent
     * states compare equals.
     */
    void addIfValid(Set<State> states, State state) {
        // Check that all chips are either shielded or on a floor
        // without another generator on.
        for (int i = 0; i < state.substances.size(); i++) {
            Substance s = state.substances.get(i);

            if (s.chipFloor != s.genFloor) {
                for (int j = 0; j < state.substances.size(); j++) {
                    if (j != i
                            && state.substances.get(j).genFloor == s.chipFloor)
                        return;
                }
            }
        }

        /*
         * Sort the list of substances. This is a critical step for the search
         * space not to explode.
         */
        Collections.sort(state.substances, new Comparator<Substance>() {
            @Override
            public int compare(Substance o1, Substance o2) {
                return ComparisonChain.start().compare(o1.genFloor, o2.genFloor)
                        .compare(o1.chipFloor, o2.chipFloor).result();
            }
        });

        states.add(state);
    }

    /**
     * Returns a collection of possible next states.
     */
    Set<State> getNextStates(State state) {
        Set<State> states = new HashSet<>();
        int elevator = state.elevator;
        int[] destFloors = new int[] { elevator + 1, elevator - 1 };

        int lowestOccupiedFloor = state.substances.stream()
                .mapToInt(s -> Math.min(s.chipFloor, s.genFloor)).min()
                .getAsInt();

        for (int destFloor : destFloors) {
            if (destFloor < lowestOccupiedFloor || destFloor > 4)
                continue;

            for (int i = 0; i < state.substances.size(); i++) {
                var si = state.substances.get(i);

                // Move chip & generator together
                if (si.chipFloor == elevator && si.genFloor == elevator) {
                    var s = copy(state, destFloor);
                    s.substances.get(i).chipFloor = destFloor;
                    s.substances.get(i).genFloor = destFloor;
                    addIfValid(states, s);
                }

                // Just chip
                if (si.chipFloor == elevator) {
                    var s = copy(state, destFloor);
                    s.substances.get(i).chipFloor = destFloor;
                    addIfValid(states, s);
                }

                // Just generator
                if (si.genFloor == elevator) {
                    var s = copy(state, destFloor);
                    s.substances.get(i).genFloor = destFloor;
                    addIfValid(states, s);
                }

                for (int j = 0; j < state.substances.size(); j++) {
                    if (j == i)
                        continue;

                    var sj = state.substances.get(j);

                    // Chip & chip (different kinds)
                    if (sj.chipFloor == elevator && si.chipFloor == elevator) {
                        var s = copy(state, destFloor);
                        s.substances.get(i).chipFloor = destFloor;
                        s.substances.get(j).chipFloor = destFloor;
                        addIfValid(states, s);
                    }

                    // Generator & generator (different kinds)
                    if (sj.genFloor == elevator && si.genFloor == elevator) {
                        var s = copy(state, destFloor);
                        s.substances.get(i).genFloor = destFloor;
                        s.substances.get(j).genFloor = destFloor;
                        addIfValid(states, s);
                    }
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
        return null;
    }

    int bfs(State input) {
        Deque<State> queue = new LinkedList<>();
        Set<State> seen = new HashSet<>();

        // Track search depth in a separate map (we cannot put a depth
        // field in the State record, as that would alter the comparison
        // semantics).
        Map<State, Integer> depthMap = new HashMap<>();

        queue.add(input);
        while (!queue.isEmpty()) {
            var s = queue.removeFirst();
            var depth = depthMap.getOrDefault(s, 0);

            if (isTargetState(s)) {
                return depth;
            }

            for (State n : getNextStates(s)) {
                if (seen.contains(n)) {
                    continue;
                } else {
                    queue.add(n);
                    seen.add(n);
                    depthMap.put(n, depth + 1);
                }
            }
        }
        throw new RuntimeException();
    }

    @Override
    public Integer part1(State input) {
        return bfs(new State(//
                List.of(new Substance(1, 1), // strontium
                        new Substance(1, 1), // plutonium
                        new Substance(2, 2), // ruthenium
                        new Substance(2, 2), // curium
                        new Substance(2, 3) // thulium
                ), 1));
    }

    @Override
    public Integer part2(State input) {
        return bfs(new State(//
                List.of(new Substance(1, 1), // strontium
                        new Substance(1, 1), // plutonium
                        new Substance(1, 1), // elerium
                        new Substance(1, 1), // dilithium
                        new Substance(2, 2), // ruthenium
                        new Substance(2, 2), // curium
                        new Substance(2, 3) // thulium
                ), 1));
    }

    public static void main(String[] args) {
        AocBaseRunner.run(new Day11());
    }
}
