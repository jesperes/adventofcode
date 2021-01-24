package aoc2016;

import java.io.File;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;
import java.util.Optional;
import java.util.TreeSet;
import java.util.function.Function;

import common2.AocBaseRunner;
import common2.AocPuzzleInfo;
import common2.AocResult;
import common2.IAocIntPuzzle;

/**
 * Day 14: One-Time Pad
 *
 * Current runtime here is 10-12 seconds (measured from BeforeClass to
 * AfterClass), which probably should be considered pretty good. ~60% of that
 * time is spent in md5.digest().
 */
public class Day14 implements IAocIntPuzzle<String> {

	common.MD5 md5 = new common.MD5();

	private byte toHexb(byte b) {
		return (byte) ((b <= 9) ? b + 48 : b + 87);
	}

	private void toHexDigest(byte[] digest, byte[] hexdigest) {
		for (int i = 0; i < 16; i++) {
			hexdigest[i * 2] = toHexb((byte) ((digest[i] & 0xf0) >> 4));
			hexdigest[i * 2 + 1] = toHexb((byte) (digest[i] & 0x0f));
		}
	}

	private byte has3(byte[] hexdigest) {
		for (int i = 0; i < hexdigest.length - 2; i++) {
			byte c = hexdigest[i];
			if (hexdigest[i + 1] == c && hexdigest[i + 2] == c)
				return c;
		}
		return -1;
	}

	private byte has5(byte[] hexdigest) {
		for (int i = 0; i < hexdigest.length - 4; i++) {
			byte c = hexdigest[i];
			if (hexdigest[i + 1] == c && hexdigest[i + 2] == c
					&& hexdigest[i + 3] == c && hexdigest[i + 4] == c) {
				return c;
			}
		}
		return -1;
	}

	// Stretched MD5 for part 2
	private byte[] stretchedMD5(byte[] b) {
		int reps = 2016;
		byte[] hexdigest = new byte[32];
		toHexDigest(md5.digest(b), hexdigest);
		for (int i = 0; i < reps; i++) {
			toHexDigest(md5.digest(hexdigest), hexdigest);
		}
		return hexdigest;
	}

	/**
	 * Improved version of day 14 solution. We do not need to compute hashes
	 * more than once (hence no memoization). Instead, we scan for 5-sequences
	 * and keep track of matching 3-sequences along the way. Typically, once we
	 * find a 5-sequence, we will be able to assign many keys based on
	 * 3-sequences found less than 1000 indexes away.
	 *
	 * @param salt The input salt, e.g. "abc" (example input).
	 * @param md5  The MD5 function. Differs between part 1 and part 2.
	 */
	private int findKey(String salt, Function<byte[], byte[]> md5) {
		// Use TreeSet, because order matters!
		Map<Character, TreeSet<Integer>> threes = new HashMap<>();
		TreeSet<Integer> keys = new TreeSet<>();

		for (int i = 0;; i++) {
			byte[] key = (salt + String.valueOf(i)).getBytes();
			byte[] keyHexDigest = md5.apply(key);

			/*
			 * Check for 5-sequences. Loop over all seen hashes with matching
			 * 3-sequences.
			 */
			byte c5 = has5(keyHexDigest);
			if (c5 != -1) {
				/*
				 * Iterate over matching 3-sequences. Using tree-sets ensures
				 * that we iterate in index order. (There will typically be
				 * several 3-sequences for each found 5-sequence, which are much
				 * more rare.)
				 */
				for (int i3 : threes.get((char) c5)) {
					int dist = Math.abs(i - i3);
					if (dist <= 1000) {
						keys.add(i3);
						if (keys.size() == 64) {
							return keys.last();
						}
					}
				}
			}

			/*
			 * Remember any 3-sequences we find, and store them so we can look
			 * them up when we encounter the matching 5-sequence.
			 */
			byte c3 = has3(keyHexDigest);
			if (c3 != -1) {
				final int i3 = i;
				threes.compute((char) c3, (k, v) -> {
					if (v == null)
						v = new TreeSet<>();
					v.add(i3);
					return v;
				});
			}
		}

	}

	@Override
	public AocPuzzleInfo getInfo() {
		return new AocPuzzleInfo(2016, 14, "One-Time Pad", false);
	}

	@Override
	public AocResult<Integer, Integer> getExpected() {
		return AocResult.of(23890, 22696);
	}

	@Override
	public String parse(Optional<File> file) throws IOException {
		return "ahsbgdzn";
	}

	@Override
	public Integer part1(String input) {
		return findKey(input, md5::hexdigest);
	}

	@Override
	public Integer part2(String input) {
		return findKey(input, this::stretchedMD5);
	}

	public static void main(String[] args) {
		AocBaseRunner.run(new Day14());
	}

	@Override
	public void dumpStats() {
		md5.dumpStats(getInfo().toString());
	}

}
