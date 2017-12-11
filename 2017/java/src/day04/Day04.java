package day04;

import static org.junit.Assert.*;

import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;
import java.util.stream.Collectors;

import org.junit.Test;

public class Day04 {

	boolean isValid(String passphrase) {
		Set<String> words = new HashSet<>();
		for (String word : passphrase.split(" ")) {
			if (words.contains(word)) {
				return false;
			} else {
				words.add(word);
			}
		}
		return true;
	}

	String createSignature(String word) {
		return Arrays.stream(word.split("")).sorted()
				.collect(Collectors.joining());
	}

	boolean isValidPart2(String passphrase) {
		Set<String> signatures = new HashSet<>();
		for (String word : passphrase.split(" ")) {
			String sign = createSignature(word);
			if (signatures.contains(sign)) {
				return false;
			} else {
				signatures.add(sign);
			}
		}

		return true;
	}

	/*
	 * -----------------------------------------------------------------------
	 * Test cases
	 * -----------------------------------------------------------------------
	 */

	@Test
	public 	void testPart1_short() throws Exception {
		assertTrue(isValid("aa bb cc dd ee"));
		assertFalse(isValid("aa bb cc dd aa"));
		assertTrue(isValid("aa bb cc dd aaa"));
	}

	@Test
	public 	void testPart1_full() throws Exception {
		try (BufferedReader reader = new BufferedReader(
				new InputStreamReader(Day04.class.getClassLoader()
						.getResourceAsStream("day04/passwords.txt")))) {
			System.out.println("[Day04]: Number of valid passwords: "
					+ reader.lines().filter(this::isValid).count());
		}
	}

	@Test
	public 	void testPart2_short() {
		assertTrue(isValidPart2("abcde fghij"));
		assertFalse(isValidPart2("abcde xyz ecdab"));
		assertTrue(isValidPart2("a ab abc abd abf abj"));
		assertTrue(isValidPart2("iiii oiii ooii oooi oooo"));
		assertFalse(isValidPart2("oiii ioii iioi iiio"));
	}

	@Test
	public void testPart2_full() throws Exception {
		try (BufferedReader reader = new BufferedReader(
				new InputStreamReader(Day04.class.getClassLoader()
						.getResourceAsStream("day04/passwords.txt")))) {
			System.out.println("[Day04]: Number of valid passwords (part 2): "
					+ reader.lines().filter(this::isValidPart2).count());
		}
	}
}
