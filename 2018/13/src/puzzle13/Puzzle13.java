package puzzle13;

import java.io.BufferedReader;
import java.io.FileReader;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;

public class Puzzle13 {

	static boolean STOP_ON_FIRST_COLLISION = false;
	static String INPUT = "input.txt";

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
			return "Cart [dir=" + dir + ", turn=" + turn + ", x=" + x + ", y=" + y + "]";
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

	public static void main(String[] args) throws Exception {

		List<Cart> carts = new ArrayList<>();

		try (BufferedReader r = new BufferedReader(new FileReader(INPUT))) {
			List<String> lines = r.lines().collect(Collectors.toList());
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

			while (numCarts(carts) > 1) {
				moveCarts(grid, carts);
			}

			Cart last = remaining(carts);

			System.out.format("Last remaining cart is at %d,%d%n", last.x, last.y);
		}
	}

	// static void printGrid(List<List<Character>> grid, List<Cart> carts) {
	// int y = 0;
	// for (List<Character> line : grid) {
	// int x = 0;
	// for (Character c : line) {
	// boolean isCart = false;
	//
	// for (Cart cart : carts) {
	// if (cart.obliterated)
	// continue;
	//
	// if (cart.x == x && cart.y == y) {
	// System.out.print(cart.dir);
	// isCart = true;
	// break;
	// }
	// }
	//
	// if (!isCart) {
	// System.out.print(c);
	// }
	//
	// x++;
	// }
	//
	// System.out.println();
	// y++;
	// }
	// }

	static Cart remaining(List<Cart> carts) {
		return carts.stream().filter(c -> !c.obliterated).findFirst().get();
	}

	static long numCarts(List<Cart> carts) {
		return carts.stream().filter(c -> !c.obliterated).count();
	}

	static void moveCarts(List<List<Character>> grid, List<Cart> carts) throws Exception {
		Collections.sort(carts);

		for (Cart cart : carts) {
			if (cart.obliterated)
				continue;

			moveCart(cart, carts, grid);
		}
	}

	private static void moveCart(Cart cart, List<Cart> carts, List<List<Character>> grid) {
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
				if (STOP_ON_FIRST_COLLISION) {
					System.out.format("Collision at %d,%d%n", cart.x, cart.y);
					System.exit(0);
				} else {
					cart.obliterated = true;
					other.obliterated = true;
				}
			}
		}

		char newGridPos = grid.get(cart.y).get(cart.x);
		switch (newGridPos) {
		case '+':
			// We've reached an intersection, we should turn according to cart.turn.
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
	}
}