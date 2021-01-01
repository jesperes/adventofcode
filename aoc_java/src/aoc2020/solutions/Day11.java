package aoc2020.solutions;

import static org.junit.Assert.assertTrue;

import java.io.InputStream;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Optional;
import java.util.function.BiFunction;

import aoc2020.AocPuzzleInfo;
import aoc2020.AocResult;
import aoc2020.IAocPuzzle;
import aoc2020.InputUtils;
import aoc2020.solutions.Day11.WaitingArea;

public class Day11 implements IAocPuzzle<WaitingArea, Long, Long> {

    record Coord(int x, int y) {
    }

    record WaitingArea(Map<Coord, State> grid) {
    }

    enum State {
        Floor('.'), Empty('L'), Occupied('#');

        char c;

        State(char c) {
            this.c = c;
        }
    }

    @Override
    public AocPuzzleInfo getInfo() {
        return new AocPuzzleInfo(2020, 11, "Seating System", true);
    }

    @Override
    public AocResult<Long, Long> getExpected() {
        return AocResult.of(2093L, 1862L);
    }

    @Override
    public WaitingArea parse(Optional<InputStream> stream) {
        List<String> list = InputUtils.asStringList(stream.get());
        Map<Coord, State> map = new HashMap<>();
        for (int y = 0; y < list.size(); y++) {
            int len = list.get(y).length();
            for (int x = 0; x < len; x++) {
                Coord coord = new Coord(x, y);
                for (State p : State.values()) {
                    if (p.c == list.get(y).charAt(x)) {
                        map.put(coord, p);
                        break;
                    }
                }
                assertTrue(map.containsKey(coord));
            }
        }
        return new WaitingArea(map);
    }

    public static WaitingArea clone(WaitingArea area) {
        WaitingArea copy = new WaitingArea(new HashMap<>());
        for (Entry<Coord, State> e : area.grid().entrySet()) {
            copy.grid().put(e.getKey(), e.getValue());
        }
        return copy;
    }

    @Override
    public Long part1(WaitingArea input) {
        var area = clone(input);
        while (doOneIteration1(area)) {
        }
        return numOccupied(area);
    }

    @Override
    public Long part2(WaitingArea input) {
        var area = clone(input);
        while (doOneIteration2(area)) {
        }
        return numOccupied(area);
    }

    private boolean doOneIteration1(WaitingArea input) {
        return doOneIteration(input, (coord, oldState) -> {
            int numAdj = occupiedAdjacents(coord, input);
            if (oldState == State.Empty && numAdj == 0) {
                return State.Occupied;
            } else if (oldState == State.Occupied && numAdj >= 4) {
                return State.Empty;
            } else {
                return oldState;
            }
        });
    }

    private boolean doOneIteration2(WaitingArea input) {
        return doOneIteration(input, (coord, oldState) -> {
            int numVis = numVisible(coord, input);
            if (oldState == State.Empty && numVis == 0) {
                return State.Occupied;
            } else if (oldState == State.Occupied && numVis >= 5) {
                return State.Empty;
            } else {
                return oldState;
            }
        });
    }

    private long numOccupied(WaitingArea input) {
        return input.grid().values().stream().filter(n -> n == State.Occupied)
                .count();
    };

    /**
     * Do one iteration.
     * 
     * @param input Input data
     * @param fun   Transition function.
     * @return True if there were any changes, false otherwise.
     */
    private boolean doOneIteration(WaitingArea input,
            BiFunction<Coord, State, State> fun) {
        // Compute changes
        Map<Coord, State> changes = new HashMap<>();
        for (Entry<Coord, State> e : input.grid().entrySet()) {
            Coord coord = e.getKey();
            State oldState = e.getValue();
            State newState = fun.apply(coord, oldState);

            if (newState != oldState) {
                changes.put(coord, newState);
            }
        }

        // Apply changes
        for (Entry<Coord, State> e : changes.entrySet()) {
            input.grid().put(e.getKey(), e.getValue());
        }

        return changes.size() > 0;
    }

    static Coord[] deltas = { //
            new Coord(-1, -1), //
            new Coord(0, -1), //
            new Coord(1, -1), //
            new Coord(-1, 0), //
            new Coord(1, 0), //
            new Coord(-1, 1), //
            new Coord(0, 1), //
            new Coord(1, 1) //
    };

    private int occupiedAdjacents(Coord c, WaitingArea area) {
        int n = 0;
        for (Coord d : deltas) {
            Coord adj = new Coord(c.x + d.x, c.y + d.y);
            if (area.grid().get(adj) == State.Occupied)
                n++;
        }
        return n;
    }

    private int numVisible(Coord c, WaitingArea input) {
        int n = 0;
        for (Coord d : deltas) {
            State state = findFirstInDirection(c, d, input);
            if (state == State.Occupied)
                n++;
        }
        return n;
    }

    private State findFirstInDirection(Coord c, Coord d, WaitingArea input) {
        for (int dist = 1;; dist++) {
            Coord c0 = new Coord(c.x + d.x * dist, c.y + d.y * dist);
            State pos = input.grid().getOrDefault(c0, null);
            if (pos == null) {
                // everything outside the grid is considered "floor"
                return State.Floor;
            }
            if (pos != State.Floor)
                return pos;
        }
    }
}
