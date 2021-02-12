package aoc2018.day15;

public class Unit implements Comparable<Unit> {
    public Pos pos;
    public boolean isElf;
    public int hp = 200;
    public int attackPower = 3;

    public Unit(Pos pos, boolean isElf, int attackPower) {
        this.pos = pos;
        this.isElf = isElf;
        this.attackPower = attackPower;
    }

    @Override
    public int compareTo(Unit o) {
        return pos.compareTo(o.pos);
    }
}