package aoc2018;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Optional;

import aoc2018.Day13.Grid;
import common2.AocBaseRunner;
import common2.AocPuzzleInfo;
import common2.AocResult;
import common2.IAocPuzzle;
import common2.InputUtils;

public class Day13 implements IAocPuzzle<Grid, String, String> {

    static int SIZE = 200;

    record Grid(char[] tracks, List<Cart> carts) {
        Grid copy() {
            char[] tracks0 = new char[SIZE * SIZE];
            System.arraycopy(tracks, 0, tracks0, 0, SIZE * SIZE);

            List<Cart> carts0 = new ArrayList<>();
            for (Cart cart : carts) {
                carts0.add(new Cart(cart.x, cart.y, cart.dir, cart.turn));
            }

            return new Grid(tracks0, carts0);
        }
    }

    record Location(int x, int y) {
    }

    static class Cart implements Comparable<Cart> {
        char dir;
        int turn; // -1, 0, 1
        int x;
        int y;
        boolean obliterated = false;

        public Cart(int x, int y, char dir, int turn) {
            super();
            this.x = x;
            this.y = y;
            this.dir = dir;
            this.turn = turn;
        }

        @Override
        public int compareTo(Cart other) {
            if (y != other.y) {
                return Integer.compare(y, other.y);
            } else {
                return Integer.compare(x, other.x);
            }

        }

        @Override
        public String toString() {
            return String.format("{y=%d,x=%d,dir=%c,turn=%d}", y, x, dir, turn);
        }
    }

    static char followTurn(char dir, char turn_type) {
        // @formatter:off
        switch (turn_type) {
        case '/':
            switch (dir) {
            case 'v': return '<';
            case '<': return 'v';
            case '>': return '^';
            case '^': return '>';
            }
        case '\\':
            switch (dir) {
            case 'v': return '>';
            case '<': return '^';
            case '>': return 'v';
            case '^': return '<';
            }
        }
        throw new RuntimeException();
        // @formatter:on
    }

    static char turn(char dir, int turn) {
        switch (dir) {
        case 'v':
            switch (turn) {
            case -1:
                return '>';
            case 0:
                return 'v';
            case 1:
                return '<';
            }
        case '^':
            switch (turn) {
            case -1:
                return '<';
            case 0:
                return '^';
            case 1:
                return '>';
            }
        case '<':
            switch (turn) {
            case -1:
                return 'v';
            case 0:
                return '<';
            case 1:
                return '^';
            }
        case '>':
            switch (turn) {
            case -1:
                return '^';
            case 0:
                return '>';
            case 1:
                return 'v';
            }
        default:
            throw new RuntimeException();
        }
    }

    /**
     * Run carts until finished. If continueOnCollision is false, returns the
     * first cart colliding (part 1). Otherwise, runs until all carts have
     * collided (and been removed), and returns the last cart left.
     */
    public Cart runCarts(Grid grid, boolean continueOnCollision) {
        int i = 1;
        while (numCarts(grid.carts) > 1) {
            Cart collided = moveCarts(grid, i++, continueOnCollision);
            if (collided != null) {
                return collided;
            }
        }

        return remaining(grid.carts);
    }

    static Cart remaining(List<Cart> carts) {
        return carts.stream().filter(c -> !c.obliterated).findFirst().get();
    }

    static long numCarts(List<Cart> carts) {
        return carts.stream().filter(c -> !c.obliterated).count();
    }

    Cart moveCarts(Grid grid, int round, boolean continueOnCollision) {
        Collections.sort(grid.carts);

        for (Cart cart : grid.carts) {
            if (cart.obliterated)
                continue;

            Cart collided = moveCart(cart, grid, continueOnCollision);
            if (collided != null) {
                return collided;
            }
        }

        return null;
    }

    private Cart moveCart(Cart cart, Grid grid, boolean continueOnCollision) {

        // @formatter:off
        switch (cart.dir) {
        case '<': cart.x--; break;
        case '>': cart.x++; break;
        case '^': cart.y--; break;
        case 'v': cart.y++; break;
        default: throw new RuntimeException();
        }
        // @formatter:on

        for (Cart other : grid.carts) {
            if (other.obliterated)
                continue;

            if (other.x == cart.x && other.y == cart.y && other != cart) {
                // Collision!

                if (continueOnCollision) {
                    cart.obliterated = true;
                    other.obliterated = true;
                } else {
                    return cart;
                }
            }
        }

        char newGridPos = grid.tracks[cart.y * SIZE + cart.x];
        switch (newGridPos) {
        case '+':
            // We've reached an intersection, we should turn according to
            // cart.turn.
            cart.dir = turn(cart.dir, cart.turn);

            // Direction to take in next intersection
            switch (cart.turn) {
            case -1:
                cart.turn = 0;
                break;
            case 0:
                cart.turn = 1;
                break;
            case 1:
                cart.turn = -1;
                break;
            default:
                throw new RuntimeException();
            }
            break;
        case '/':
        case '\\':
            // We've reached a turn in the track
            cart.dir = followTurn(cart.dir, newGridPos);
            break;
        case '-':
        case '|':
            // Nothing to do, we just continue in the same direction.
            break;
        default:
            throw new RuntimeException();
        }

        return null;
    }

    @Override
    public AocPuzzleInfo getInfo() {
        return new AocPuzzleInfo(2018, 13, "Mine Cart Madness", true);
    }

    @Override
    public AocResult<String, String> getExpected() {
        return AocResult.of("94,78", "26,85");

    }

    @Override
    public Grid parse(Optional<File> file) throws IOException {
        List<Cart> carts = new ArrayList<>();
        char[] grid = new char[SIZE * SIZE];

        int y = 0;
        for (String line : InputUtils.asStringList(file.get())) {
            for (int x = 0; x < line.length(); x++) {
                char c = line.charAt(x);
                switch (c) {
                case 'v':
                case '^':
                    carts.add(new Cart(x, y, c, -1));
                    grid[y * SIZE + x] = '|';
                    break;
                case '<':
                case '>':
                    carts.add(new Cart(x, y, c, -1));
                    grid[y * SIZE + x] = '-';
                    break;
                default:
                    grid[y * SIZE + x] = c;
                    break;
                }
            }

            y++;
        }
        return new Grid(grid, carts);
    }

    @Override
    public String part1(Grid input) {
        Cart cart = runCarts(input.copy(), false);
        return "%d,%d".formatted(cart.x, cart.y);
    }

    @Override
    public String part2(Grid input) {
        Cart cart = runCarts(input.copy(), true);
        return "%d,%d".formatted(cart.x, cart.y);
    }

    public static void main(String[] args) {
        AocBaseRunner.run(new Day13());
    }
}
