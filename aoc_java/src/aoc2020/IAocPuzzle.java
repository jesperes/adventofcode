package aoc2020;

import java.io.BufferedReader;
import java.util.Optional;

/**
 * 
 * @param T type of the parsed input
 * @param V result type (normally Integer)
 * @return
 */
public interface IAocPuzzle<TInput, TPart1, TPart2> {

	/**
	 * If {@link AocPuzzleInfo#hasInputFile} is false, reader will be empty here,
	 * and the method should just return the input given in the puzzle.
	 * 
	 * @param reader
	 * @return
	 */
	public TInput parse(Optional<BufferedReader> reader);

	public TPart1 part1(TInput input);

	public TPart2 part2(TInput input);

	public AocResult<TPart1, TPart2> getExpected();

	public AocPuzzleInfo getInfo();

	default public String describe() {
		AocPuzzleInfo info = getInfo();
		return String.format("%d day %d: %s", info.year, info.day, info.name);
	}
}
