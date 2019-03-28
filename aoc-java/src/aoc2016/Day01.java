package aoc2016;

import static org.junit.Assert.assertEquals;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.HashSet;
import java.util.Set;
import java.util.stream.Collectors;

import org.junit.Test;

/**
 * --- Day 1: No Time for a Taxicab ---
 * 
 * Santa's sleigh uses a very high-precision clock to guide its movements, and
 * the clock's oscillator is regulated by stars. Unfortunately, the stars have
 * been stolen... by the Easter Bunny. To save Christmas, Santa needs you to
 * retrieve all fifty stars by December 25th.
 * 
 * Collect stars by solving puzzles. Two puzzles will be made available on each
 * day in the advent calendar; the second puzzle is unlocked when you complete
 * the first. Each puzzle grants one star. Good luck!
 * 
 * You're airdropped near Easter Bunny Headquarters in a city somewhere. "Near",
 * unfortunately, is as close as you can get - the instructions on the Easter
 * Bunny Recruiting Document the Elves intercepted start here, and nobody had
 * time to work them out further.
 * 
 * The Document indicates that you should start at the given coordinates (where
 * you just landed) and face North. Then, follow the provided sequence: either
 * turn left (L) or right (R) 90 degrees, then walk forward the given number of
 * blocks, ending at a new intersection.
 * 
 * There's no time to follow such ridiculous instructions on foot, though, so
 * you take a moment and work out the destination. Given that you can only walk
 * on the street grid of the city, how far is the shortest path to the
 * destination?
 * 
 * For example:
 * 
 * Following R2, L3 leaves you 2 blocks East and 3 blocks North, or 5 blocks
 * away.
 * 
 * R2, R2, R2 leaves you 2 blocks due South of your starting position, which is
 * 2 blocks away.
 * 
 * R5, L5, R5, R3 leaves you 12 blocks away. How many blocks away is Easter
 * Bunny HQ?
 * 
 * @author jesperes
 *
 */
public class Day01 {

    private String[] getInput() throws IOException {
        try (BufferedReader reader = new BufferedReader(
                new InputStreamReader(Day01.class.getClassLoader()
                        .getResourceAsStream("day01/input.txt")))) {
            return reader.lines().collect(Collectors.joining()).split(", +");
        }
    }

    class Position {
        /**
         * Direction is represented as an int { 0, 1, 2, 3 } where N is 0 going
         * clockwise.
         */
        int dir = 0;
        int x = 0;
        int y = 0;

        public Position() {
        }

        public Position(int dir, int x, int y) {
            this.dir = dir;
            this.x = x;
            this.y = y;
        }

        // Returns an array of positions visited during the execution
        public Position[] execute(Instruction instr) {
            Position visited[] = new Position[instr.steps];
            dir = (dir + instr.turn + 4) % 4;
            switch (dir) {
            case 0: // North
                for (int i = 0; i < instr.steps; i++) {
                    visited[i] = new Position(0, x, y - (i + 1));
                }
                break;
            case 1: // East
                for (int i = 0; i < instr.steps; i++) {
                    visited[i] = new Position(0, x + (i + 1), y);
                }
                break;
            case 2: // South
                for (int i = 0; i < instr.steps; i++) {
                    visited[i] = new Position(0, x, y + (i + 1));
                }
                break;
            case 3: // West
                for (int i = 0; i < instr.steps; i++) {
                    visited[i] = new Position(0, x - (i + 1), y);
                }
                break;
            }

            // Move ourselves to the last position visited.
            x = visited[instr.steps - 1].x;
            y = visited[instr.steps - 1].y;

            return visited;
        }

        public int getDistance() {
            return Math.abs(x) + Math.abs(y);
        }
    }

    class Instruction {
        /** Turns are represented as -1/1 */
        int turn;
        int steps;

        public Instruction(String p) {
            turn = p.substring(0, 1).equals("L") ? -1 : 1;
            steps = Integer.valueOf(p.substring(1, p.length()));
        }
    }

    private int getNumberOfSteps(String[] path) {
        Position pos = new Position();

        for (String p : path) {
            pos.execute(new Instruction(p));
        }

        return pos.getDistance();
    }

    private int getNumberOfSteps_Part2(String[] path) {
        Set<String> visitedPositions = new HashSet<>();
        Position pos = new Position();

        for (String p : path) {
            for (Position visited : pos.execute(new Instruction(p))) {
                String s = String.format("%d,%d", visited.x, visited.y);

                if (visitedPositions.contains(s)) {
                    return visited.getDistance();
                } else {
                    visitedPositions.add(s);
                }
            }
        }

        return -1;
    }

    @Test
    public void testPart1_short() throws Exception {
        assertEquals(5, getNumberOfSteps(new String[] { "R2", "L3" }));
        assertEquals(2, getNumberOfSteps(new String[] { "R2", "R2", "R2" }));
        assertEquals(12,
                getNumberOfSteps(new String[] { "R5", "L5", "R5", "R3" }));
    }

    @Test
    public void testPart2_short() throws Exception {
        assertEquals(4, getNumberOfSteps_Part2(
                new String[] { "R8", "R4", "R4", "R8" }));
    }

    @Test
    public void testPart1_full() throws Exception {
        System.out
                .println("[Day01]: Distance: " + getNumberOfSteps(getInput()));
    }

    @Test
    public void testPart2_full() throws Exception {
        System.out.println("[Day01]: Distance (part 2): "
                + getNumberOfSteps_Part2(getInput()));
    }

}
