package aoc2018.day15;

import com.google.common.hash.HashCode;

/**
 * A single position, comparable in reading order.
 */
public class Pos implements Comparable<Pos> {
    public int x;
    public int y;

    public Pos(int x, int y) {
        this.x = x;
        this.y = y;
    }

    @Override
    public int hashCode() {
        return HashCode.fromInt(x ^ y).asInt();
    }

    @Override
    public boolean equals(Object obj) {
        Pos pos = (Pos) obj;
        return x == pos.x && y == pos.y;
    }

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

}