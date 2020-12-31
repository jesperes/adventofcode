package aoc2020;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.stream.Collectors;

import aoc2020.solutions.Day01;
import aoc2020.solutions.Day02;
import aoc2020.solutions.Day03;
import aoc2020.solutions.Day04;
import aoc2020.solutions.Day15;

public class Aoc2020 {

	public static void main(String[] args) throws IOException {
		List<IAocPuzzle<?, ?, ?>> puzzles = new ArrayList<>();

		// =====================================================
		puzzles.add(new Day01());
		puzzles.add(new Day02());
		puzzles.add(new Day03());
		puzzles.add(new Day04());
		puzzles.add(new Day15());
		// =====================================================

		List<AocPuzzleRun<?, ?, ?>> runs = runPuzzles(puzzles);
		printTable(runs);
	}

	private static void printTable(List<AocPuzzleRun<?, ?, ?>> runs) throws IOException {
		String sep = "@";
		StringBuilder cmd = new StringBuilder();

		AocResultTableField[] columns = new AocResultTableField[] { //
				AocResultTableField.Year, //
				AocResultTableField.Day, //
				AocResultTableField.Name, //
				AocResultTableField.Parsing, //
				AocResultTableField.Part1Time, //
				AocResultTableField.Part2Time, //
				AocResultTableField.Part1Result, //
				AocResultTableField.Part2Result, //
				AocResultTableField.Part1Status, //
				AocResultTableField.Part2Status //
		};

		// Headers
		cmd.append(Arrays.stream(columns).map(col -> col.label).collect(Collectors.joining(sep)) + "\n");

		for (AocPuzzleRun<?, ?, ?> run : runs) {
			Map<AocResultTableField, String> map = run.toTableRow();
			cmd.append(Arrays.stream(columns).map(col -> map.get(col)).collect(Collectors.joining(sep)) + "\n");
		}

		cmd.append(Arrays.stream(columns).map(col -> {
			switch (col) {
			case Name:
				return "Total";
			case Parsing:
				return String.format("%.3f secs", runs.stream().map((run) -> {
					if (run.puzzle().getInfo().hasInputFile) {
						return run.result().timing().get().parsing();
					} else {
						return 0L;
					}
				}).collect(Collectors.summingLong(n -> n)) / 1000000000.0);
			case Part1Time:
				return String.format("%.3f secs", runs.stream().map(run -> run.result().timing().get().part1())
						.collect(Collectors.summingLong(n -> n)) / 1000000000.0);
			case Part2Time:
				return String.format("%.3f secs", runs.stream().map(run -> run.result().timing().get().part2())
						.collect(Collectors.summingLong(n -> n)) / 1000000000.0);
			default:
				return "";
			}
		}).collect(Collectors.joining(sep)) + "\n");

		Path path = Files.createTempFile("tabulate", ".txt");
		try {
			Files.writeString(path, cmd.toString());
			ProcessBuilder pb = new ProcessBuilder("tabulate", "-f", "grid", "-s", sep, path.toString());
			pb.inheritIO();
			Process p = pb.start();
			p.waitFor();
		} catch (InterruptedException e) {
			e.printStackTrace();
		} finally {
			Files.delete(path);
		}
	}

	private static List<AocPuzzleRun<?, ?, ?>> runPuzzles(List<IAocPuzzle<?, ?, ?>> puzzles) throws IOException {
		List<AocPuzzleRun<?, ?, ?>> runs = new ArrayList<>();
		for (IAocPuzzle<?, ?, ?> puzzle : puzzles) {
			runs.add(run(puzzle));
		}
		return runs;
	}

	private static <T, P1, P2> AocPuzzleRun<T, P1, P2> run(IAocPuzzle<T, P1, P2> puzzle) throws IOException {
		AocPuzzleInfo info = puzzle.getInfo();
		File inputFile = null;
		AocResult<P1, P2> result;

		if (info.hasInputFile) {
			inputFile = new File(String.format("inputs/%d/input%02d.txt", info.year, info.day));
			if (!inputFile.exists()) {
				throw new FileNotFoundException("Input file is missing: " + inputFile);
			}

			try (BufferedReader reader = new BufferedReader(new FileReader(inputFile))) {
				result = runWithInput(Optional.of(reader), puzzle);
			}
		} else {
			result = runWithInput(Optional.empty(), puzzle);
		}

		return new AocPuzzleRun<T, P1, P2>(puzzle, result);
	}

	private static <T, P1, P2> AocResult<P1, P2> runWithInput(Optional<BufferedReader> reader,
			IAocPuzzle<T, P1, P2> puzzle) {
		long t0 = System.nanoTime();
		T input = puzzle.parse(reader);
		long t1 = System.nanoTime();
		P1 p1 = puzzle.part1(input);
		long t2 = System.nanoTime();
		P2 p2 = puzzle.part2(input);
		long t3 = System.nanoTime();
		long t_parsing = t1 - t0;
		long t_part1 = t2 - t1;
		long t_part2 = t3 - t2;

		return AocResult.of(p1, p2, new AocTiming(t_parsing, t_part1, t_part2));
	}
}
