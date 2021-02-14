package aoc2018.day15;

import static org.junit.Assert.assertNotNull;

import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

/**
 * Holds the cavern layout and the list of units present.
 */
public class Cavern {
    public Map<Pos, Character> map = new HashMap<>();
    public Map<Pos, Unit> units = new HashMap<>();
    int width = 0;
    int height = 0;
    public boolean allowElfDeath = true;

    public Cavern(String grid) {
        this(grid, 3);
    }

    public Cavern(String grid, int elfAP) {
        int goblinAP = 3; // goblin attack power is always 3
        int y = 0;
        for (String line : grid.split("\n")) {
            int x = 0;
            width = line.length();
            for (char c : line.trim().toCharArray()) {
                Pos pos = new Pos(x, y);
                // @formatter:off
                switch (c) {
                case '#': map.put(pos, c); break;
                case 'G': units.put(pos, new Unit(pos, false, goblinAP)); break;
                case 'E': units.put(pos, new Unit(pos, true, elfAP)); break;
                case '.': break;
                default: throw new RuntimeException();
                }
                // @formatter:on
                x++;
            }
            height++;
            y++;
        }
    }

    public boolean isWall(Pos pos) {
        return map.containsKey(pos);
    }

    public boolean isUnit(Pos pos) {
        return units.containsKey(pos) && units.get(pos).hp > 0;
    }

    public boolean isOpen(Pos pos) {
        return !isWall(pos) && !isUnit(pos);
    }

    public void move(Unit unit, Pos newPos) {
        assertNotNull(newPos);
        units.remove(unit.pos);
        unit.pos = newPos;
        units.put(newPos, unit);
    }

    /**
     * Return a list of all (live) units, sorted in reading order.
     */
    public List<Unit> getSortedUnits() {
        return units.entrySet().stream().filter(e -> e.getValue().hp > 0)
                .map(e -> e.getValue()).sorted().collect(Collectors.toList());
    }

    /**
     * Return a list of all (live) enemies to the given unit.
     */
    public List<Unit> getEnemies(Unit unit) {
        return units.entrySet().stream().map(e -> e.getValue())
                .filter(u -> u.hp > 0 && u.isElf != unit.isElf)
                .collect(Collectors.toList());
    }

    public List<Unit> getAdjacentEnemies(Unit unit) {
        return units.entrySet().stream().map(e -> e.getValue())
                .filter(u -> u.hp > 0 && u.isElf != unit.isElf
                        && minCost(u.pos, unit.pos) == 1)
                .collect(Collectors.toList());
    }

    int minCost(Pos a, Pos b) {
        return Math.abs(a.x() - b.x()) + Math.abs(a.y() - b.y());
    }

    public String toString() {
        return toString(Collections.emptyList(), (char) 0, null);
    }

    public String toString(Collection<Pos> coll, char c, Pos mark) {
        StringBuilder sb = new StringBuilder();
        for (int y = 0; y < height; y++) {
            sb.append(String.format("%2d ", y));

            for (int x = 0; x < width; x++) {
                Pos pos = new Pos(x, y);
                if (map.containsKey(pos)) {
                    sb.append("#");
                } else if (mark != null && pos.equals(mark)) {
                    sb.append('+');
                } else if (coll.contains(pos)) {
                    sb.append(c);
                } else if (units.containsKey(pos)) {
                    sb.append(units.get(pos).isElf ? "E" : "G");
                } else {
                    sb.append(".");
                }
            }

            int y0 = y;

            sb.append(" ");

            sb.append(units.values().stream().filter(unit -> unit.pos.y() == y0)
                    .sorted(new Comparator<Unit>() {
                        @Override
                        public int compare(Unit o1, Unit o2) {
                            return o1.pos.compareTo(o2.pos);
                        }
                    }).map(unit -> unit.toString())
                    .collect(Collectors.joining(", ")));

            sb.append("\n");
        }
        return sb.toString();
    }
}