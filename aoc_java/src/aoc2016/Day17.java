package aoc2016;

import static java.lang.Math.abs;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Optional;

import common.AStar;
import common.AStar.IAStarCallbacks;
import common.MD5;
import common2.AocBaseRunner;
import common2.AocPuzzleInfo;
import common2.AocResult;
import common2.IAocPuzzle;

public class Day17 implements IAocPuzzle<String, String, Integer> {

    MD5 md5 = new MD5();

    record Pos(int x, int y, String str) {
    }

    @Override
    public AocPuzzleInfo getInfo() {
        return new AocPuzzleInfo(2016, 17, "Two Steps Forward", false);
    }

    @Override
    public AocResult<String, Integer> getExpected() {
        return AocResult.of("DDRRUDLRRD", 488);
    }

    @Override
    public String parse(Optional<File> file) throws IOException {
        return "pslxynzg";
    }

    @Override
    public String part1(String input) {
        var list = AStar.astar(List.of(new Pos(0, 0, "")),
                new IAStarCallbacks<Pos>() {
                    @Override
                    public int heuristic(Pos node) {
                        return abs(node.x - 3) + abs(node.y - 3);
                    }

                    @Override
                    public boolean isGoal(Pos node) {
                        return node.x == 3 && node.y == 3;
                    }

                    @Override
                    public Collection<Pos> neighbors(Pos node) {
                        return Day17.this.neighbors(node, input);
                    }
                });
        return list.get(list.size() - 1).str();
    }

    @Override
    public Integer part2(String input) {
        return dfs(new Pos(0, 0, ""), input);
    }

    int dfs(Pos pos, String input) {
        if (pos.x == 3 && pos.y == 3) {
            return pos.str.length();
        } else {
            return neighbors(pos, input).stream()
                    .mapToInt(node -> dfs(node, input)).max()
                    .orElseGet(() -> 0);
        }
    }

    /*
     * Helpers
     */

    private Collection<Pos> neighbors(Pos node, String input) {
        List<Pos> list = new ArrayList<>();
        byte[] checksum = md5.hexdigest((input + node.str).getBytes());
        addIfOpen(list, (char) checksum[0], node.str, "U", node.x, node.y - 1);
        addIfOpen(list, (char) checksum[1], node.str, "D", node.x, node.y + 1);
        addIfOpen(list, (char) checksum[2], node.str, "L", node.x - 1, node.y);
        addIfOpen(list, (char) checksum[3], node.str, "R", node.x + 1, node.y);
        return list;
    }

    private void addIfOpen(List<Pos> list, char b, String str, String c, int x,
            int y) {
        if (b >= 'b' && x >= 0 && y >= 0 && x <= 3 && y <= 3) {
            list.add(new Pos(x, y, str + c));
        }
    }

    public static void main(String[] args) {
        AocBaseRunner.run(new Day17());
    }

    @Override
    public void dumpStats() {
        md5.dumpStats(getInfo().toString());
    }
}
