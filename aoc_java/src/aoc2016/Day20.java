package aoc2016;

import java.io.File;
import java.io.IOException;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

import com.google.common.collect.ComparisonChain;

import aoc2016.Day20.IPRange;
import common2.AocBaseRunner;
import common2.AocPuzzleInfo;
import common2.AocResult;
import common2.IAocPuzzle;
import common2.InputUtils;

public class Day20 implements IAocPuzzle<List<IPRange>, Long, Integer> {

	static final long MAX_IP = 4294967295L;

	record IPRange(long start, long end) implements Comparable<IPRange> {
		@Override
		public int compareTo(IPRange o) {
			return ComparisonChain.start().compare(start, o.start)
					.compare(end, o.end).result();
		}

		boolean contains(long ip) {
			return ip >= start && ip <= end;
		}
	}

	@Override
	public AocPuzzleInfo getInfo() {
		return new AocPuzzleInfo(2016, 20, "Firewall Rules", true);
	}

	@Override
	public AocResult<Long, Integer> getExpected() {
		return AocResult.of(17348574L, 104);
	}

	@Override
	public List<IPRange> parse(Optional<File> file) throws IOException {
		return InputUtils.asStringList(file.get()).stream().map(line -> {
			String[] elems = line.split("-");
			return new IPRange(Long.parseLong(elems[0]),
					Long.parseLong(elems[1]));
		}).sorted().collect(Collectors.toUnmodifiableList());
	}

	/**
	 * Part 1: Find lowest non-blocked IP
	 */
	@Override
	public Long part1(List<IPRange> input) {
		return findLowestNonBlockedIP(input, 0L);
	}

	/**
	 * Find the lowest non-blocked IP starting larger than a given starting
	 * point.
	 */
	long findLowestNonBlockedIP(List<IPRange> input, long start) {
		long ip = start;
		while (true) {
			var range = findFirstRange(input, ip);
			if (range.isEmpty()) {
				return ip;
			} else {
				ip = range.get().end + 1;
			}
		}
	}

	/**
	 * Find first range which blocks the given ip.
	 */
	Optional<IPRange> findFirstRange(List<IPRange> input, long ip) {
		for (var range : input) {
			if (range.contains(ip))
				return Optional.of(range);
		}
		return Optional.empty();
	}

	/**
	 * Part 2: Find the number of allowed IPs
	 */
	@Override
	public Integer part2(List<IPRange> input) {
		long ip = 0L;
		int numAllowedIPs = 0;
		while (true) {
			ip = findLowestNonBlockedIP(input, ip);
			if (isValid(ip)) {
				numAllowedIPs++;
				ip++;
			} else {
				return numAllowedIPs;
			}
		}
	}

	boolean isValid(long ip) {
		return ip <= MAX_IP;
	}

	public static void main(String[] args) {
		AocBaseRunner.run(new Day20());
	}
}
