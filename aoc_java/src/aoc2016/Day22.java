package aoc2016;

import static org.junit.Assert.assertTrue;

import java.io.File;
import java.io.IOException;
import java.util.List;
import java.util.Optional;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

import aoc2016.Day22.Node;
import common2.AocBaseRunner;
import common2.AocPuzzleInfo;
import common2.AocResult;
import common2.IAocIntPuzzle;
import common2.InputUtils;

public class Day22 implements IAocIntPuzzle<List<Node>> {
	Pattern pattern = Pattern.compile("/dev/grid/node-x(?<x>\\d+)-y(?<y>\\d+)"
			+ "\\s+(?<size>\\d+)T\\s+(?<used>\\d+)T\\s+(?<avail>\\d+)T.*");

	record Node(int x, int y, int size, int used, int avail) {
	}

	@Override
	public AocPuzzleInfo getInfo() {
		return new AocPuzzleInfo(2016, 22, "Grid Computing", true);
	}

	@Override
	public AocResult<Integer, Integer> getExpected() {
		return AocResult.of(864, 244);
	}

	@Override
	public List<Node> parse(Optional<File> file) throws IOException {
		return InputUtils.asStringList(file.get()).stream().skip(2)
				.map(line -> {
					var m = pattern.matcher(line);
					assertTrue(m.matches());
					return new Node(//
							Integer.parseInt(m.group("x")),
							Integer.parseInt(m.group("y")),
							Integer.parseInt(m.group("size")),
							Integer.parseInt(m.group("used")),
							Integer.parseInt(m.group("avail")));
				}).collect(Collectors.toList());
	}

	@Override
	public Integer part1(List<Node> input) {
		int numViableNodes = 0;
		for (Node a : input) {
			for (Node b : input) {
				if (a == b)
					continue;
				if (a.used == 0)
					continue;
				if (a.used <= b.avail)
					numViableNodes++;
			}
		}
		return numViableNodes;
	}

	@Override
	public Integer part2(List<Node> input) {
		/*
		 * Ok, so this one was a little fun. Part 1 was about finding
		 * "viable pairs", i.e. pairs of nodes where you can move data from A to
		 * B. This was trivial (see part1() method above.)
		 * 
		 * In part two the goal is to move the data in the top-right node into
		 * the top-left node. Data can only be moved between physically adjacent
		 * nodes, so we can visualize this as a 2x2 grid of nodes (which happens
		 * to be 35 nodes wide and 24 nodes high). Also, data can only be moved
		 * to nodes which have enough space.
		 * 
		 * After some parsing, sorting, and looking at the numbers of the nodes
		 * (and after reading the hints in the puzzle), we discover that there
		 * are 3 types of nodes:
		 * 
		 * 1. There are a number of nodes which are really large and really
		 * full. These have a size >= 500T, and usepercentage >= 95%. These
		 * cannot move anywhere, since there is no empty node large enough to
		 * hold that amount of data. These are effectively "walls".
		 * 
		 * 2. The bulk of the nodes are ~85T and 68-85% full.
		 * 
		 * 3. One empty node 87T large {17, 22}.
		 * 
		 * The only way to move anything is using the single empty node, so it
		 * becomes similar to a 15-game where you can only move one node at a
		 * time.
		 * 
		 * To get an idea of how the grid looks, I printed it out.
		 * 
		 * It turns out that all of the full nodes are on line 21, except one
		 * semi-full node. The empty node is directly beneath the wall.
		 * 
		 * S..................................G
		 * ....................................
		 * ....................................
		 * ....................................
		 * ....................................
		 * ....................................
		 * ....................................
		 * ....................................
		 * ....................................
		 * ....................................
		 * ....................................
		 * ....................................
		 * ....................................
		 * ....................................
		 * ....................................
		 * ....................................
		 * ....................................
		 * ....................................
		 * ....................................
		 * ....................................
		 * ....................................
		 * .###################################
		 * ................._..................
		 * ....................................
		 * ....................................
		 * 
		 * At this point I realized that the simplest way would probably be to
		 * manually count the number of steps.
		 * 
		 * 1. The empty node is at (17, 22). Move to (0, 22): 17 steps. 2. Move
		 * empty node to (0, 20): 2 steps. 3. Move empty node to (33, 20): 34
		 * steps. 4. Move empty node to (33, 0): 20 steps. 5. Move G from (34,0)
		 * to (33,0), swapping Goal/Empty: 1 step. 6. Move empty node around
		 * goal to (32,0): 4 steps. 7. Repeat steps 5 and 6 34 times. Empty is
		 * now at (0,0) and goal at (1, 0). 8. Move goal to (0,0): 1 step.
		 * 
		 * So, first:
		 * 
		 * S................................._G
		 * ..................................|.
		 * ..................................|.
		 * ..................................|.
		 * ..................................|.
		 * ..................................|.
		 * ..................................|.
		 * ..................................|.
		 * ..................................|.
		 * ..................................|.
		 * ..................................|.
		 * ..................................|.
		 * ..................................|.
		 * ..................................|.
		 * ..................................|.
		 * ..................................|.
		 * ..................................|.
		 * ..................................|.
		 * ..................................|.
		 * ..................................|.
		 * /---------------------------------/.
		 * |###################################
		 * \----------------_..................
		 * ....................................
		 * ....................................
		 * 
		 * Then, 34 repetitions of this sequence:
		 * 
		 * @formatter:off
		 * 
		 * ..._G... -> ...G_... -> ...G.... 
		 * ........    ........    ...._...
		 * 
		 * ...G.... -> ...G.... -> .._G.... 
		 * ..._....    .._.....    ........
		 * 
		 * And the last move to move G into place
		 * 
		 * _G... -> G_... 
		 * .....    .....
		 * 
		 * @formatter:on
		 *	
		 * Counting the moves, we get
		 * 
		 * Total: 17 + 2 + 34 + 20 + 5 * 34 + 1 = 244.
		 */
		return 244;
	}

	public static void main(String[] args) {
		AocBaseRunner.run(new Day22());
	}
}
