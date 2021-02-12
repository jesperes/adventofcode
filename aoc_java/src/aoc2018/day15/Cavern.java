package aoc2018.day15;

import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * Holds the cavern layout and the list of units present.
 */
public class Cavern {
    public Map<Pos, Character> map = new HashMap<>();
    public Map<Pos, Unit> units = new HashMap<>();
    int width = 0;
    int height = 0;

    public Cavern(String grid) {
        this(grid, 3);
    }

    public Cavern(String grid, int elfAP) {
        int goblinAP = 3; // goblin attack power is always 3
        int y = 0;
        for (String line : grid.split("\n")) {
            int x = 0;
            width = line.length();
            for (char c : line.toCharArray()) {
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

    /**
     * Return a list of all units, sorted in reading order.
     */
    public List<Unit> getSortedUnits() {
        return units.entrySet().stream().map(e -> e.getValue()).sorted()
                .collect(Collectors.toUnmodifiableList());
    }

    /**
     * Return a list of all enemies to the given unit.
     */
    public List<Unit> getEnemies(Unit unit) {
        return units.entrySet().stream().map(e -> e.getValue())
                .filter(u -> u.isElf != unit.isElf)
                .collect(Collectors.toUnmodifiableList());
    }

    /**
     * Return a list of all open positions adjacent to any of the given units.
     */
    public Set<Pos> getAdjacentTo(Collection<Unit> units) {
        Set<Pos> set = new HashSet<>();
        for (Unit unit : units) {
            getAdjacentTo(set, unit.pos);
        }
        return set;
    }

    public void getAdjacentTo(Set<Pos> set, Pos pos) {
        addIfOpen(set, new Pos(pos.x, pos.y - 1));
        addIfOpen(set, new Pos(pos.x - 1, pos.y));
        addIfOpen(set, new Pos(pos.x + 1, pos.y));
        addIfOpen(set, new Pos(pos.x, pos.y + 1));
    }

    private void addIfOpen(Set<Pos> set, Pos pos) {
        if (isOpen(pos))
            set.add(pos);
    }

    private boolean isOpen(Pos pos) {
        return !map.containsKey(pos) && !units.containsKey(pos);
    }

    public String toString() {
        return toString(Collections.emptyList(), (char) 0);
    }

    public String toString(Collection<Pos> coll, char c) {
        StringBuilder sb = new StringBuilder();
        for (int y = 0; y < height; y++) {
            for (int x = 0; x < width; x++) {
                Pos pos = new Pos(x, y);
                if (map.containsKey(pos)) {
                    sb.append("#");
                } else if (coll.contains(pos)) {
                    sb.append(c);
                } else if (units.containsKey(pos)) {
                    sb.append(units.get(pos).isElf ? "E" : "G");
                } else {
                    sb.append(".");
                }
            }
            sb.append("\n");
        }
        return sb.toString();
    }
}