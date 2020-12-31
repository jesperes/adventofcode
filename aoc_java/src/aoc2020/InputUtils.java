package aoc2020;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.StringReader;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

public class InputUtils {

	/**
	 * Return strings from a {@link BufferedReader} a list of longs.
	 * 
	 * @param reader
	 * @return
	 */
	public static List<Long> asLongList(BufferedReader reader) {
		List<Long> list = new ArrayList<>();
		String line;
		try {
			while ((line = reader.readLine()) != null) {
				list.add(Long.valueOf(line));
			}
		} catch (IOException e) {
			throw new RuntimeException(e);
		}
		return list;
	}

	public static List<String> asStringList(BufferedReader reader) {
		List<String> list = new ArrayList<>();
		String line;
		try {
			while ((line = reader.readLine()) != null) {
				list.add(line);
			}
		} catch (IOException e) {
			throw new RuntimeException(e);
		}
		return list;
	}

	public static Optional<BufferedReader> withReaderFromString(String s) {
		return Optional.of(new BufferedReader(new StringReader(s)));
	}
}
