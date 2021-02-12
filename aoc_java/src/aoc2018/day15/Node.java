package aoc2018.day15;

import com.google.common.collect.ComparisonChain;
import com.google.common.hash.HashCode;

public class Node implements Comparable<Node> {
    public Pos start;
    public Pos pos;
    public int manhattanDistToTarget;
    public int stepsFromStart;

    public Node(Pos start, Pos pos, int toTarget, int fromStart) {
        this.start = start;
        this.pos = pos;
        this.manhattanDistToTarget = toTarget;
        this.stepsFromStart = fromStart;
    }

    @Override
    public int hashCode() {
        return HashCode.fromInt(start.hashCode() ^ pos.hashCode()).asInt();
    }

    @Override
    public boolean equals(Object obj) {
        Node node = (Node) obj;
        return start.equals(node.start) && pos.equals(node.pos)
                && manhattanDistToTarget == node.manhattanDistToTarget;
    }

    @Override
    public int compareTo(Node o) {
        return ComparisonChain.start() //
                .compare(manhattanDistToTarget, o.manhattanDistToTarget) //
                .compare(pos, o.pos) //
                .compare(start, o.start).result();
    }

    @Override
    public String toString() {
        return "{start=%s,pos=%s,toTarget=%d,fromStart=%d}".formatted(start,
                pos, manhattanDistToTarget, stepsFromStart);
    }

}