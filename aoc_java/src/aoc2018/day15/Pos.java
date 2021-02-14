package aoc2018.day15;

import java.util.HashSet;
import java.util.Set;

/**
 * A single position, comparable in reading order.
 */
public record Pos(int x, int y) implements Comparable<Pos> {

    @Override
    public int compareTo(Pos o) {
        if (y != o.y) {
            return Integer.compare(y, o.y);
        } else {
            return Integer.compare(x, o.x);
        }
    }

    @Override
    public String toString() {
        return "{%d,%d}".formatted(x, y);
    }

    public Set<Pos> getNeighbors() {
        Set<Pos> set = new HashSet<>();
        set.add(new Pos(x, y - 1));
        set.add(new Pos(x - 1, y));
        set.add(new Pos(x + 1, y));
        set.add(new Pos(x, y + 1));
        return set;
    }
}