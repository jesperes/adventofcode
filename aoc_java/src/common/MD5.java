package common;

import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;

public class MD5 {
	static MessageDigest md5 = init();

	static private MessageDigest init() {
		try {
			return MessageDigest.getInstance("MD5");
		} catch (NoSuchAlgorithmException e) {
			throw new RuntimeException(e);
		}
	}

	public static byte[] digest(byte[] bytes) {
		return md5.digest();
	}

	public static byte[] digest(String str) {
		return md5.digest(str.getBytes());
	}

	public static byte[] hexdigest(byte[] bytes) {
		byte[] hexdigest = new byte[32];
		toHexDigest(bytes, hexdigest);
		return hexdigest;
	}

	public static String hexdigest(String string) {
		byte[] hexdigest = new byte[32];
		toHexDigest(digest(string), hexdigest);
		return new String(hexdigest);
	}

	private static byte toHexb(byte b) {
		return (byte) ((b <= 9) ? b + 48 : b + 87);
	}

	private static void toHexDigest(byte[] digest, byte[] hexdigest) {
		for (int i = 0; i < 16; i++) {
			hexdigest[i * 2] = toHexb((byte) ((digest[i] & 0xf0) >> 4));
			hexdigest[i * 2 + 1] = toHexb((byte) (digest[i] & 0x0f));
		}
	}
}
