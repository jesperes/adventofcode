package aoc2020;

import static org.junit.Assert.assertEquals;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import org.junit.Test;

import common.AocPuzzle;

public class Day01 extends AocPuzzle {

	private List<Integer> list;

	public Day01() throws IOException {
		super(2020, 1);

		list = new ArrayList<Integer>();
		getInputAsIntStream().forEach(n -> {
			list.add(n);
		});

	}

	private long part1(List<Integer> list) {
		for (int x : list) {
			for (int y : list) {
				if (x < y)
					continue;

				if (x + y == 2020) {
					return x * y;
				}
			}
		}
		return 0;
	}

	private long part2(List<Integer> list) {
		for (int x : list) {
			for (int y : list) {
				if (x < y || x + y >= 2020)
					continue;

				for (int z : list) {
					if (x + y + z == 2020) {
						return x * y * z;
					}
				}
			}
		}
		return 0;
	}

	@Test
	public void testPart1() throws Exception {
		assertEquals(987339, part1(list));
	}

	@Test
	public void testPart2() throws Exception {
		assertEquals(259521570, part2(list));
	}
}
