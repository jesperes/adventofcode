package aoc2020.solutions;

import static org.junit.Assert.assertTrue;

import java.io.File;
import java.io.FileNotFoundException;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.function.Function;

import aoc2020.AocBaseRunner;
import aoc2020.AocPuzzleInfo;
import aoc2020.AocResult;
import aoc2020.IAocPuzzle;
import aoc2020.InputUtils;

public class Day24 implements IAocPuzzle<List<String>, Integer, Integer> {

    record HexCoord(int x, int y, int z) {
    }

    record HexDir(String prefix, Function<HexCoord, HexCoord> moveFun) {
    }

    static Map<HexCoord, Set<HexCoord>> cache = new HashMap<>();

    record Tiles(Set<HexCoord> tiles) {

        public boolean isBlack(HexCoord tile) {
            return tiles.contains(tile);
        }

        public boolean isWhite(HexCoord tile) {
            return !tiles.contains(tile);
        }

        public void flip(HexCoord tile) {
            if (isBlack(tile)) {
                tiles.remove(tile);
            } else {
                tiles.add(tile);
            }
        }

        public int numBlacks() {
            return tiles.size();
        }

        public Set<HexCoord> getTilesToCheck() {
            Set<HexCoord> set = new HashSet<>();
            set.addAll(tiles);
            for (HexCoord c : tiles) {
                set.addAll(getAdjacents(c));
            }
            return set;
        }

        static public Set<HexCoord> getAdjacents(HexCoord c) {
            return cache.computeIfAbsent(c, v -> computeAdjacents(v));
        }

        static private Set<HexCoord> computeAdjacents(HexCoord c) {
            Set<HexCoord> nbrs = new HashSet<>();
            for (int dx = -1; dx <= 1; dx++) {
                for (int dy = -1; dy <= 1; dy++) {
                    for (int dz = -1; dz <= 1; dz++) {
                        nbrs.add(new HexCoord(c.x + dx, c.y + dy, c.z + dz));
                    }
                }
            }
            return nbrs;
        }

        public int countBlackAdjacents(HexCoord c) {
            int n = 0;
            for (int dx = -1; dx <= 1; dx++) {
                for (int dy = -1; dy <= 1; dy++) {
                    for (int dz = -1; dz <= 1; dz++) {
                        if (dx == 0 && dy == 0 && dz == 0)
                            continue;

                        if (dx + dy + dz == 0) {
                            if (isBlack(new HexCoord(c.x + dx, c.y + dy,
                                    c.z + dz))) {
                                n++;
                            }
                        }
                    }
                }
            }
            return n;
        }
    }

    static HexDir[] hexDirs = new HexDir[] {
            // the two-letter ones should be first
            // @formatter:off
            new HexDir("ne", (c -> new HexCoord(c.x + 1, c.y,     c.z - 1))),
            new HexDir("nw", (c -> new HexCoord(c.x,     c.y + 1, c.z - 1))),
            new HexDir("se", (c -> new HexCoord(c.x,     c.y - 1, c.z + 1))),
            new HexDir("sw", (c -> new HexCoord(c.x - 1, c.y,     c.z + 1))),
            new HexDir("e",  (c -> new HexCoord(c.x + 1, c.y - 1, c.z    ))),
            new HexDir("w",  (c -> new HexCoord(c.x - 1, c.y + 1, c.z    )))
            // @formatter:on
    };

    @Override
    public AocPuzzleInfo getInfo() {
        return new AocPuzzleInfo(2020, 24, "Lobby Layout", true);
    }

    @Override
    public AocResult<Integer, Integer> getExpected() {
        return AocResult.of(300, 3466);
    }

    @Override
    public List<String> parse(Optional<File> file) {
        return InputUtils.asStringList(file.get());
    }

    static private Tiles flipTiles(List<String> input) {
        Tiles tiles = new Tiles(new HashSet<>());

        for (String line : input) {
            flipTiles(line, tiles);
        }

        return tiles;
    }

    static private boolean hasSubstringAt(String line, String prefix, int at) {
        for (int i = 0; i < prefix.length(); i++) {
            if (line.charAt(at + i) != prefix.charAt(i))
                return false;
        }
        return true;
    }

    static private void flipTiles(String line, Tiles tiles) {
        HexCoord c = new HexCoord(0, 0, 0);

        for (int i = 0; i < line.length(); /* nop */) {
            boolean found = false;
            for (HexDir dir : hexDirs) {
                if (hasSubstringAt(line, dir.prefix, i)) {
                    c = dir.moveFun.apply(c);
                    i += dir.prefix.length();
                    found = true;
                    break;
                }
            }
            assertTrue(found);
        }

        tiles.flip(c);
    }

    @Override
    public Integer part1(List<String> input) {
        Tiles tiles = flipTiles(input);
        return tiles.numBlacks();
    }

    @Override
    public Integer part2(List<String> input) {
        Tiles tiles = flipTiles(input);

        Set<HexCoord> toFlip = new HashSet<>();

        for (int i = 0; i < 100; i++) {
            toFlip.clear();

            for (HexCoord c : tiles.getTilesToCheck()) {
                int numBlacks = tiles.countBlackAdjacents(c);

                if (tiles.isBlack(c)) {
                    if (numBlacks == 0 || numBlacks > 2)
                        toFlip.add(c);
                } else {
                    if (numBlacks == 2)
                        toFlip.add(c);
                }
            }

            for (HexCoord c : toFlip) {
                tiles.flip(c);
            }
        }

        return tiles.numBlacks();
    }

    public static void main(String[] args) throws FileNotFoundException {
        AocBaseRunner.run(new Day24());
    }
}
