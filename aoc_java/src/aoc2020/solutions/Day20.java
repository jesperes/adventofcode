package aoc2020.solutions;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;

import java.io.File;
import java.io.FileNotFoundException;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.junit.Test;

import com.google.common.collect.Multimap;
import com.google.common.collect.MultimapBuilder;

import aoc2020.AocBaseRunner;
import aoc2020.AocPuzzleInfo;
import aoc2020.AocResult;
import aoc2020.Coord;
import aoc2020.IAocIntPuzzle;
import aoc2020.InputUtils;
import aoc2020.solutions.Day20.Jigsaw;

public class Day20 implements IAocIntPuzzle<Jigsaw> {

    static final int SIZE = 10; // all tiles are 10x10
    static final int NORTH = 0;
    static final int EAST = 1;
    static final int SOUTH = 2;
    static final int WEST = 3;

    /**
     * Id of a tile, containing the original tile id plus a symmetry label. By
     * separating these, we can track which edge ids belong to which tiles
     * modulo symmetries.
     */
    record TileId(int id, String sym) {
        public String toString() {
            return String.format("%d-%s", id, sym);
        }
    }

    record Tile(TileId tileId, char[][] pixels) {
        public String toString() {
            StringBuilder s = new StringBuilder("Tile " + tileId.id + ":\n");

            for (int y = 0; y < SIZE; y++) {
                for (int x = 0; x < SIZE; x++) {
                    s.append(pixels[y][x]);
                }
                s.append("\n");
            }
            return s.toString();
        }
    }

    static Tile makeTile(String s) {
        char[][] pixels = new char[SIZE][SIZE];
        String[] elems = s.split(":");
        TileId id = new TileId(Integer.parseInt(elems[0]), "0");
        int y = 0;
        for (String line : elems[1].trim().split("\n")) {
            int x = 0;
            for (char c : line.trim().toCharArray()) {
                pixels[y][x] = c;
                x++;
            }
            y++;
        }
        return new Tile(id, pixels);
    }

    static char[][] copyMatrix(char matrix[][]) {
        char[][] copy = new char[matrix.length][matrix.length];
        for (int y = 0; y < matrix.length; y++) {
            for (int x = 0; x < matrix.length; x++) {
                copy[y][x] = matrix[y][x];
            }
        }
        return copy;
    }

    static Tile copyTile(Tile original) {
        return new Tile(new TileId(original.tileId.id, original.tileId.sym),
                copyMatrix(original.pixels));
    }

    static char[][] rotate(char matrix[][]) {
        var size = matrix.length;

        // first find the transpose of the matrix.
        for (int i = 0; i < size; i++) {
            for (int j = i; j < size; j++) {
                var temp = matrix[i][j];
                matrix[i][j] = matrix[j][i];
                matrix[j][i] = temp;
            }
        }

        // reverse each row
        for (int i = 0; i < size; i++) {
            for (int j = 0; j < size / 2; j++) {
                var temp = matrix[i][j];
                matrix[i][j] = matrix[i][size - 1 - j];
                matrix[i][size - 1 - j] = temp;
            }
        }

        return matrix;
    }

    static char[][] flip(char matrix[][]) {
        var size = matrix.length;
        for (int i = 0; i < size; i++) {
            for (int j = 0; j < size / 2; j++) {
                var temp = matrix[i][j];
                matrix[i][j] = matrix[i][size - 1 - j];
                matrix[i][size - 1 - j] = temp;
            }
        }

        return matrix;
    }

    static Tile rotateTile(Tile tile) {
        return new Tile(new TileId(tile.tileId.id, tile.tileId.sym + "r90"),
                rotate(tile.pixels));
    }

    static Tile flipTile(Tile tile) {
        return new Tile(new TileId(tile.tileId.id, tile.tileId.sym + "f"),
                flip(tile.pixels));
    }

    static private int[] computeEdgeIds(Tile tile) {
        int[] edges = new int[] { 1 << 16, 1 << 16, 1 << 16, 1 << 16 };

        /*
         * Edge ids are converted to integers by interpreting the edges as
         * binary numbers. MSB is in the direction of positive coordinates. This
         * has the additional benefit of edge ids uniquely identifying the
         * orientation of a tile.
         */
        for (int i = 0; i < 10; i++) {
            if (tile.pixels[0][i] == '#')
                edges[NORTH] |= (1 << i);

            if (tile.pixels[i][SIZE - 1] == '#')
                edges[EAST] |= (1 << i);

            if (tile.pixels[SIZE - 1][i] == '#')
                edges[SOUTH] |= (1 << i);

            if (tile.pixels[i][0] == '#')
                edges[WEST] |= (1 << i);
        }

        return edges;
    }

    record Edges(TileId tileId, int[] ids) {
    }

    class Jigsaw {
        Map<TileId, Tile> tiles;

        // Maps tile ids to their edge ids
        Map<TileId, Edges> edgeMap;

        // Maps edge ids to the tile ids which they belong. Keys which map
        // to multiple tile ids are "internal" edges (they "connect" two tiles).
        Multimap<Integer, Integer> invEdgeMap;

        public Jigsaw(Map<TileId, Tile> tiles) {
            this.tiles = tiles;

            edgeMap = tiles.values().stream()
                    .collect(Collectors.toMap(t -> t.tileId,
                            t -> new Edges(t.tileId, computeEdgeIds(t))));

            invEdgeMap = MultimapBuilder.hashKeys().hashSetValues().build();

            edgeMap.values().stream()
                    .forEach(edges -> Arrays.stream(edges.ids).forEach(
                            edgeid -> invEdgeMap.put(edgeid, edges.tileId.id)));
        }
    }

    @Override
    public AocPuzzleInfo getInfo() {
        return new AocPuzzleInfo(2020, 20, "Jurassic Jigsaw", true);
    }

    @Override
    public AocResult<Long, Long> getExpected() {
        return AocResult.of(66020135789767L, 1537L);
    }

    @Override
    public Jigsaw parse(Optional<File> file) {
        Map<TileId, Tile> tileMap = Arrays
                .stream(InputUtils.asString(file.get()).split("Tile "))
                .filter(s -> s.length() > 0).map(s -> makeTile(s))
                .flatMap(this::withSymmetries)
                .collect(Collectors.toMap(t -> t.tileId, t -> t));

        return new Jigsaw(tileMap);
    }

    Stream<Tile> withSymmetries(Tile tile) {
        Tile r90 = rotateTile(copyTile(tile));
        Tile r180 = rotateTile(copyTile(r90));
        Tile r270 = rotateTile(copyTile(r180));
        Tile flip = flipTile(copyTile(tile));
        Tile flip90 = rotateTile(copyTile(flip));
        Tile flip180 = rotateTile(copyTile(flip90));
        Tile flip270 = rotateTile(copyTile(flip180));
        return Stream.of(tile, r90, r180, r270, flip, flip90, flip180, flip270);
    }

    @Override
    public Long part1(Jigsaw input) {
        Set<Integer> cornerTiles = findCornerTiles(input);
        return cornerTiles.stream().mapToLong(n -> n).reduce((a, b) -> a * b)
                .getAsLong();
    }

    private Set<Integer> findCornerTiles(Jigsaw input) {
        Set<Integer> externalEdges = findExternalEdges(input);

        // Corner tiles are tiles which have 2 external edges.
        Set<Integer> cornerTiles = new HashSet<>();
        for (TileId x : input.tiles.keySet()) {
            // For each tile, count the number of external edges
            if (Arrays.stream(input.edgeMap.get(x).ids)
                    .filter(id -> externalEdges.contains(id)).count() == 2) {
                cornerTiles.add(x.id);
            }
        }
        return cornerTiles;
    }

    private Set<Integer> findExternalEdges(Jigsaw input) {
        // Compute the set of external edges
        Set<Integer> externalEdges = new HashSet<>();
        for (Entry<Integer, Collection<Integer>> x : input.invEdgeMap.asMap()
                .entrySet()) {
            if (x.getValue().size() == 1) {
                externalEdges.add(x.getKey());
            }
        }
        return externalEdges;
    }

    private TileId findNorthWestTile(Jigsaw input) {
        Set<Integer> externalEdges = findExternalEdges(input);

        for (TileId x : input.tiles.keySet()) {
            Edges edges = input.edgeMap.get(x);

            if (externalEdges.contains(edges.ids[NORTH])
                    && externalEdges.contains(edges.ids[WEST])) {
                return x;
            }
        }

        throw new RuntimeException();
    }

    private void placeTile(TileId tileId, Coord coord,
            Map<Coord, TileId> placedTiles, Set<TileId> remainingTiles) {

        /*
         * Remember to remove all the symmetries of a tile when placing it, or
         * we are going to lay the same tile several times.
         */
        placedTiles.put(coord, tileId);
        remainingTiles.removeIf(t -> t.id == tileId.id);
    }

    /**
     * Part 2 is where we stitch together all the tiles and find the sea
     * monsters. This code is a bit rough around the edges, but it is still
     * pretty efficient due to the lookup maps (Jigsaw.edgeMap and
     * Jigsaw.invEdgeMap) which allows us to directly lookup the next tile to
     * place.
     */
    @Override
    public Long part2(Jigsaw input) {
        var n = (int) Math.sqrt(input.tiles.size() / 8);
        var size = n * 8;
        var nwTileId = findNorthWestTile(input); // Start with the NW tile
        Map<Coord, TileId> placedTiles = new HashMap<>();
        Set<TileId> remainingTiles = new HashSet<>();
        remainingTiles.addAll(input.tiles.keySet());

        var current = nwTileId;
        var coord = new Coord(0, 0);

        /*
         * Start at the NW corners, and lay the tiles left to right. When
         * reaching the east edge, rewind and start again with the next row.
         * When we have reached the last tile we are done.
         */
        /*
         * TODO cleanup this loop
         */
        placeTile(current, coord, placedTiles, remainingTiles);

        while (true) {
            if (coord.x() == n - 1) {
                if (coord.y() == n - 1) {
                    break; // We're done
                }

                int south = input.edgeMap.get(
                        placedTiles.get(new Coord(0, coord.y()))).ids[SOUTH];
                current = lookupNextTile(input, remainingTiles, NORTH, south);
                coord = new Coord(0, coord.y() + 1);
            } else {
                int east = input.edgeMap.get(current).ids[EAST];
                current = lookupNextTile(input, remainingTiles, WEST, east);
                coord = new Coord(coord.x() + 1, coord.y());
            }

            placeTile(current, coord, placedTiles, remainingTiles);
        }

        var sea = stitchTiles(input, n, size, placedTiles);

        return findSeaMonster(sea);
    }

    /**
     * Find a tile among the remaining ones which has the specified edgeId in
     * the specified direction (NORTH, EAST, SOUTH, WEST).
     */
    TileId lookupNextTile(Jigsaw input, Set<TileId> remainingTiles, int dir,
            int edgeId) {
        return remainingTiles.stream()
                .filter(tileId -> input.edgeMap.get(tileId).ids[dir] == edgeId)
                .findFirst().get();
    }

    private char[][] stitchTiles(Jigsaw input, int n, int size,
            Map<Coord, TileId> placedTiles) {
        char[][] sea = new char[size][size];

        /*
         * Stitch the tiles together, removing the borders.
         */
        for (int y = 0; y < n; y++) {
            for (int x = 0; x < n; x++) {
                TileId tileId = placedTiles.get(new Coord(x, y));
                Tile tile = input.tiles.get(tileId);

                for (int yy = 0; yy < SIZE - 2; yy++) {
                    for (int xx = 0; xx < SIZE - 2; xx++) {

                        // The coordinate within the tile
                        Coord sourceCoord = new Coord(xx + 1, yy + 1);

                        // The coordinate in the large sea-grid
                        Coord targetCoord = new Coord(x * (SIZE - 2) + xx,
                                y * (SIZE - 2) + yy);

                        sea[targetCoord.y()][targetCoord.x()] = //
                                tile.pixels[sourceCoord.y()][sourceCoord.x()];
                    }
                }
            }
        }
        return sea;
    }

    private Long findSeaMonster(char[][] sea) {
        /*
         * What a feeble sea monster which is threatened by something so mundane
         * as a code formatter?!
         */

        // @formatter:off
        String seaMonster =
           "                  # \n" +
           "#    ##    ##    ###\n" +
           " #  #  #  #  #  #   \n";
        // @formatter:on

        Map<Coord, Character> seaMonsterMap = new HashMap<>();
        int y = 0;
        for (String line : seaMonster.split("\n")) {
            int x = 0;
            for (char c : line.toCharArray()) {
                if (c == '#')
                    seaMonsterMap.put(new Coord(x, y), c);
                x++;
            }
            y++;
        }

        var s1 = sea;
        var s2 = rotate(copyMatrix(sea));
        var s3 = rotate(copyMatrix(s2));
        var s4 = rotate(copyMatrix(s3));
        var s5 = flip(copyMatrix(sea));
        var s6 = rotate(copyMatrix(s5));
        var s7 = rotate(copyMatrix(s6));
        var s8 = rotate(copyMatrix(s7));

        return Stream.of(s1, s2, s3, s4, s5, s6, s7, s8)
                .map(seaSym -> numSeaMonsterFreePixels(seaSym, seaMonsterMap))
                .collect(Collectors.summingLong(x -> x));
    }

    private int numSeaMonsterFreePixels(char[][] sea,
            Map<Coord, Character> seaMonsterMap) {
        int foundSeaMonsters = 0;

        Set<Coord> seaPixels = new HashSet<>();
        for (int y = 0; y < sea.length; y++) {
            for (int x = 0; x < sea.length; x++) {
                if (sea[y][x] == '#')
                    seaPixels.add(new Coord(x, y));
            }
        }

        int seaMonsterWidth = 20;
        int seaMonsterHeight = 3;

        for (int y = -seaMonsterHeight; y < sea.length
                + seaMonsterHeight; y++) {
            for (int x = -seaMonsterWidth; x < sea.length
                    + seaMonsterWidth; x++) {

                boolean matches = true;

                /*
                 * Check all pixels in the sea monster
                 */
                for (Entry<Coord, Character> e : seaMonsterMap.entrySet()) {
                    int xx = x + e.getKey().x();
                    int yy = y + e.getKey().y();
                    if (xx < 0 || xx >= sea.length || yy < 0
                            || yy >= sea.length) {
                        // part of sea monster is outside the sea
                        matches = false;
                        break;
                    } else {
                        if (sea[yy][xx] != '#') {
                            matches = false;
                            break;
                        }
                    }
                }

                if (matches) {
                    foundSeaMonsters++;
                    for (Entry<Coord, Character> e : seaMonsterMap.entrySet()) {
                        int xx = x + e.getKey().x();
                        int yy = y + e.getKey().y();

                        Coord c = new Coord(xx, yy);
                        seaPixels.remove(c);
                    }
                }
            }
        }

        if (foundSeaMonsters > 0) {
            return seaPixels.size();
        } else {
            return 0;
        }
    }

    @Test
    public void testMakeTile() throws Exception {
        Tile tile = makeTile("""
                2311:
                ..##.#..#.
                ##..#.....
                #...##..#.
                ####.#...#
                ##.##.###.
                ##...#.###
                .#.#.#..##
                ..#....#..
                ###...#.#.
                ..###..###
                """);
        System.out.println(tile);
        assertEquals(2311, tile.tileId.id);
        int[] edgeIds = computeEdgeIds(tile);
        rotateTile(tile);
        rotateTile(tile);
        rotateTile(tile);
        rotateTile(tile);
        int[] edgeIds2 = computeEdgeIds(tile);
        assertArrayEquals(edgeIds, edgeIds2);

        // Flip R-L (around vertical)
        flipTile(tile);
        int[] edgeIds3 = computeEdgeIds(tile);
        assertEquals(edgeIds3[EAST], edgeIds[WEST]);

        System.out.println(tile);
        System.out.println(Arrays.toString(edgeIds3));

    }

    @Test
    public void testSymmetries() throws Exception {
        Tile tile = makeTile("""
                2311:
                ..##.#..#.
                ##..#.....
                #...##..#.
                ####.#...#
                ##.##.###.
                ##...#.###
                .#.#.#..##
                ..#....#..
                ###...#.#.
                ..###..###
                """);

        withSymmetries(tile).forEach(t -> {
            System.out.println(t);
        });
    }

    public static void main(String[] args) throws FileNotFoundException {
        AocBaseRunner.run(new Day20());
    }
}
