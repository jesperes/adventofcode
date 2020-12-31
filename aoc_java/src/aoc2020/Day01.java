package aoc2020;

import java.io.BufferedReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

public class Day01 implements IAocPuzzle<List<Long>, Long, Long> {

	@Override
	public AocPuzzleInfo getInfo() {
		return new AocPuzzleInfo(2020, 1, "Report Repair");
	}

	@Override
	public List<Long> parse(Optional<BufferedReader> reader) {
		// return reader.get().lines().mapToLong(s ->
		// Long.valueOf(s)).boxed().collect(Collectors.toList());
		List<Long> list = new ArrayList<>();
		String line;
		try {
			while ((line = reader.get().readLine()) != null) {
				list.add(Long.valueOf(line));
			}
		} catch (IOException e) {
			throw new RuntimeException(e);
		}
		return list;
	}

	@Override
	public Long part1(List<Long> list) {
		for (long x : list) {
			for (long y : list) {
				if (x < y)
					continue;

				if (x + y == 2020) {
					return x * y;
				}
			}
		}
		return 0L;
	}

	@Override
	public Long part2(List<Long> list) {
		for (long x : list) {
			for (long y : list) {
				if (x < y || x + y >= 2020)
					continue;

				for (long z : list) {
					if (x + y + z == 2020) {
						return (long) (x * y * z);
					}
				}
			}
		}
		return 0L;
	}

	@Override
	public AocResult<Long, Long> getExpected() {
		return new AocResult<>(987339L, 259521570L, Optional.empty());
	}
}
