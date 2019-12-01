package aoc2018;

import static org.junit.Assert.assertEquals;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.junit.Test;

import common.AocPuzzle;

public class Day13 extends AocPuzzle {

    public Day13() {
        super(2018, 13);
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

    static char follow_turn(char dir, char turn_type) {
        switch (turn_type) {
        case '/':
            switch (dir) {
            case 'v':
                return '<';
            case '<':
                return 'v';
            case '>':
                return '^';
            case '^':
                return '>';
            }
            break;
        case '\\':
            switch (dir) {
            case 'v':
                return '>';
            case '<':
                return '^';
            case '>':
                return 'v';
            case '^':
                return '<';
            }
            break;
        }

        return 99;
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
        }

        return '?';
    }

    /**
     * Run carts until finished. If continueOnCollision is false, returns the
     * first cart colliding (part 1). Otherwise, runs until all carts have
     * collided (and been removed), and returns the last cart left.
     */
    public Cart runCarts(boolean continueOnCollision) throws Exception {

        List<Cart> carts = new ArrayList<>();

        List<String> lines = getInputAsLines();
        List<List<Character>> grid = new ArrayList<>();

        int y = 0;

        for (String line : lines) {

            char[] chars = line.toCharArray();
            List<Character> gridline = new ArrayList<>();

            for (int i = 0; i < chars.length; i++) {
                char c = chars[i];

                switch (c) {
                case 'v':
                case '^':
                    carts.add(new Cart(i, y, c, -1));
                    chars[i] = '|';
                    break;

                case '<':
                case '>':
                    carts.add(new Cart(i, y, c, -1));
                    chars[i] = '-';
                    break;
                default:
                    break;
                }
                gridline.add(chars[i]);
            }

            grid.add(gridline);
            y++;
        }

        int i = 1;
        while (numCarts(carts) > 1) {
            Cart collided = moveCarts(grid, carts, i++, continueOnCollision);
            if (collided != null) {
                return collided;
            }
        }

        return remaining(carts);
    }

    static Cart remaining(List<Cart> carts) {
        return carts.stream().filter(c -> !c.obliterated).findFirst().get();
    }

    static long numCarts(List<Cart> carts) {
        return carts.stream().filter(c -> !c.obliterated).count();
    }

    Cart moveCarts(List<List<Character>> grid, List<Cart> carts, int round,
            boolean continueOnCollision) throws Exception {
        Collections.sort(carts);

        for (Cart cart : carts) {
            if (cart.obliterated)
                continue;

            Cart collided = moveCart(cart, carts, grid, continueOnCollision);
            if (collided != null) {
                return collided;
            }
        }

        return null;
    }

    private Cart moveCart(Cart cart, List<Cart> carts,
            List<List<Character>> grid, boolean continueOnCollision)
            throws IOException {
        switch (cart.dir) {
        case '<':
            cart.x--;
            break;
        case '>':
            cart.x++;
            break;
        case '^':
            cart.y--;
            break;
        case 'v':
            cart.y++;
            break;
        }

        for (Cart other : carts) {
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

        char newGridPos = grid.get(cart.y).get(cart.x);
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
            }
            break;
        case '/':
        case '\\':
            // We've reached a turn in the track
            cart.dir = follow_turn(cart.dir, newGridPos);
            break;
        case '-':
        case '|':
            // Nothing to do, we just continue in the same direction.
            break;
        }

        return null;
    }

    @Test
    public void testPart1() throws Exception {
        Cart cart = runCarts(false);
        assertEquals(94, cart.x);
        assertEquals(78, cart.y);
    }

    @Test
    public void testPart2() throws Exception {
        Cart cart = runCarts(true);
        assertEquals(26, cart.x);
        assertEquals(85, cart.y);
    }
}
