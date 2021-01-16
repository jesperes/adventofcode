package aoc2016;

import java.io.File;
import java.io.IOException;
import java.util.Optional;

import common2.AocBaseRunner;
import common2.AocPuzzleInfo;
import common2.AocResult;
import common2.IAocIntPuzzle;

public class Day19 implements IAocIntPuzzle<Integer> {
	static class Elf {
		final int num;
		int presents = 1;
		Elf next = null;
		Elf prev = null;

		Elf(int n) {
			num = n;
			next = this;
			prev = this;
		}

		void insertAfter(Elf elf) {
			elf.next = next;
			elf.prev = this;
			next.prev = elf;
			next = elf;
		}

		void remove() {
			prev.next = next;
			next.prev = prev;
		}

		void steal(Elf elf) {
			presents += elf.presents;
			elf.remove();
		}

		boolean isAlone() {
			return next == this;
		}
	}

	private static Elf create_table(int num) {
		Elf curr = new Elf(1);
		Elf first = curr;
		for (int i = 2; i <= num; i++) {
			curr.insertAfter(new Elf(i));
			curr = curr.next;
		}
		return first;
	}

	@Override
	public AocPuzzleInfo getInfo() {
		return new AocPuzzleInfo(2016, 19, "An Elephant Named Joseph", false);
	}

	@Override
	public AocResult<Integer, Integer> getExpected() {
		return AocResult.of(1834471, 1420064);
	}

	@Override
	public Integer parse(Optional<File> file) throws IOException {
		return 3014387;
	}

	@Override
	public Integer part1(Integer elves) {
		Elf curr = create_table(elves);

		while (!curr.isAlone()) {
			curr.steal(curr.next);
			curr = curr.next;
		}

		return curr.num;
	}

	@Override
	public Integer part2(Integer elves) {
		Elf curr = create_table(elves);
		Elf across = curr;
		int half = elves / 2;
		for (int i = 0; i < half; i++) {
			across = across.next;
		}

		while (!curr.isAlone()) {
			curr.steal(across);
			curr = curr.next;

			// update the across ref
			across = across.next;
			if (elves % 2 == 1)
				across = across.next;
			elves--;
		}

		return curr.num;
	}

	public static void main(String[] args) {
		AocBaseRunner.run(new Day19());
	}
}
