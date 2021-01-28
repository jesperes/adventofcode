package aoc2017;

import java.io.File;
import java.io.IOException;
import java.util.Arrays;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

import aoc2017.Day11.Direction;
import common2.AocBaseRunner;
import common2.AocPuzzleInfo;
import common2.AocResult;
import common2.IAocIntPuzzle;
import common2.InputUtils;

/**
 * https://www.redblobgames.com/grids/hexagons/
 */
public class Day11 implements IAocIntPuzzle<List<Direction>> {

    enum Direction {
        nw, ne, sw, se, n, s
    }

    static class Position {
        int x = 0;
        int y = 0;
        int z = 0;

        public Position(int x, int y, int z) {
            this.x = x;
            this.y = y;
        }

        public Position(Position other) {
            this.x = other.x;
            this.y = other.y;
            this.z = other.z;
        }

        @Override
        public String toString() {
            return String.format("(%d,%d,%d)", x, y, z);
        }

        public static void move(Position pos, Direction dir) {
            switch (dir) {
            case n:
                pos.y++;
                pos.z--;
                break;
            case s:
                pos.y--;
                pos.z++;
                break;
            case ne:
                pos.z--;
                pos.x++;
                break;
            case sw:
                pos.z++;
                pos.x--;
                break;
            case nw:
                pos.y++;
                pos.x--;
                break;
            case se:
                pos.y--;
                pos.x++;
                break;
            default:
                break;
            }
        }

        public int distanceTo(Position o) {
            return (Math.abs(x - o.x) + Math.abs(y - o.y) + Math.abs(z - o.z))
                    / 2;
        }
    }

    record Result(Position pos, int dist, int maxDist) {
    }

    Result computeDistance(List<Direction> turns) {
        Position pos = new Position(0, 0, 0);
        Position startPos = new Position(0, 0, 0);

        int maxDist = Integer.MIN_VALUE;

        for (Direction dir : turns) {
            Position.move(pos, dir);
            int currentDistance = pos.distanceTo(startPos);
            if (currentDistance > maxDist) {
                maxDist = currentDistance;
            }
        }
        int dist = pos.distanceTo(startPos);

        return new Result(pos, dist, maxDist);
    }

    @Override
    public AocPuzzleInfo getInfo() {
        return new AocPuzzleInfo(2017, 11, "Hex Ed", true);
    }

    @Override
    public AocResult<Integer, Integer> getExpected() {
        return AocResult.of(685, 1457);
    }

    @Override
    public List<Direction> parse(Optional<File> file) throws IOException {
        return Arrays.stream(InputUtils.asString(file.get()).split(","))
                .map(str -> Direction.valueOf(str))
                .collect(Collectors.toUnmodifiableList());
    }

    @Override
    public Integer part1(List<Direction> input) {
        Result result = computeDistance(input);
        return result.dist;
    }

    @Override
    public Integer part2(List<Direction> input) {
        Result result = computeDistance(input);
        return result.maxDist;
    }

    public static void main(String[] args) {
        AocBaseRunner.run(new Day11());
    }
}
