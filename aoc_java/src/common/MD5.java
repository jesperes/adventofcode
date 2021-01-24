package common;

import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.Locale;

/**
 * Wrapper for MD5 calculations for AdventOfCode puzzles. The MD5 calculations
 * is often a bottleneck for the puzzles which uses it (e.g. 2016 day 14).
 * 
 * FastMD5 (http://www.twmacinta.com/myjava/fast_md5.php) claims to be a fast
 * MD5 implementations, but it does not perform well for these cases (possibly
 * because the amount of data checksummed is often small).
 */
public class MD5 {
	MessageDigest md5 = init();
	int count = 0;
	long elapsed = 0;

	private MessageDigest init() {
		try {
			return MessageDigest.getInstance("MD5");
		} catch (NoSuchAlgorithmException e) {
			throw new RuntimeException(e);
		}
	}

	public byte[] digest(byte[] bytes) {
		long t0 = System.nanoTime();
		try {
			return md5.digest(bytes);
		} finally {
			elapsed += (System.nanoTime() - t0);
			count++;
		}
	}

	public byte[] digest(String str) {
		return digest(str.getBytes());
	}

	public byte[] hexdigest(byte[] bytes) {
		byte[] hexdigest = new byte[32];
		toHexDigest(digest(bytes), hexdigest);
		return hexdigest;
	}

	private byte toHexb(byte b) {
		return (byte) ((b <= 9) ? b + 48 : b + 87);
	}

	private void toHexDigest(byte[] digest, byte[] hexdigest) {
		for (int i = 0; i < 16; i++) {
			hexdigest[i * 2] = toHexb((byte) ((digest[i] & 0xf0) >> 4));
			hexdigest[i * 2 + 1] = toHexb((byte) (digest[i] & 0x0f));
		}
	}

	public void dumpStats(String msg) {
		/*
		 * We cannot say here anything about the total running time since
		 * puzzles are run multiple times.
		 */
		System.out.format(Locale.ROOT,
				"%s: %.2fM MD5 calls (%.3f usecs/call)%n", msg,
				count / 1000000.0, (elapsed / count) / 1000.0);
	}
}
