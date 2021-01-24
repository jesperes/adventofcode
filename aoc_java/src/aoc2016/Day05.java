package aoc2016;

import java.io.File;
import java.io.IOException;
import java.util.Optional;

import common.MD5;
import common2.AocBaseRunner;
import common2.AocPuzzleInfo;
import common2.AocResult;
import common2.IAocPuzzle;

public class Day05 implements IAocPuzzle<String, String, String> {
	MD5 md5 = new MD5();

	@Override
	public AocPuzzleInfo getInfo() {
		return new AocPuzzleInfo(2016, 5, "How About a Nice Game of Chess?",
				false);
	}

	@Override
	public AocResult<String, String> getExpected() {
		return AocResult.of("4543c154", "1050cbbd");
	}

	@Override
	public String parse(Optional<File> file) throws IOException {
		return "ojvtpuvg";
	}

	private byte toHexb(byte b) {
		return (byte) ((b <= 9) ? b + 48 : b + 87);
	}

	private boolean hasLeadingZeroes(String input, int n, byte[] hexdigest) {
		String s = input + Integer.toString(n);
		byte[] digest = md5.digest(s.getBytes());
		if (digest[0] == 0 && digest[1] == 0 && (digest[2] & 0xf0) == 0) {
			for (int i = 0; i < 16; i++) {
				hexdigest[i * 2] = toHexb((byte) ((digest[i] & 0xf0) >> 4));
				hexdigest[i * 2 + 1] = toHexb((byte) (digest[i] & 0x0f));
			}

			return true;
		} else {
			return false;
		}
	}

	@Override
	public String part1(String input) {
		StringBuilder pwd = new StringBuilder();
		byte[] hexdigest = new byte[32];
		for (int i = 0; pwd.length() < 8; i++) {
			if (hasLeadingZeroes(input, i, hexdigest)) {
				pwd.append((char) hexdigest[5]);
			}
		}
		return pwd.toString();
	}

	@Override
	public String part2(String input) {
		byte[] pwd = new byte[8];
		byte[] hexdigest = new byte[32];

		for (int i = 0, n = 0; n < 8; i++) {
			if (hasLeadingZeroes(input, i, hexdigest)) {
				char pos = (char) hexdigest[5];
				if (pos >= '0' && pos <= '7' && pwd[pos - '0'] == 0) {
					pwd[pos - '0'] = hexdigest[6];
					n++;
				}
			}
		}
		return new String(pwd);
	}

	public static void main(String[] args) {
		AocBaseRunner.run(new Day05());
	}

	@Override
	public void dumpStats() {
		md5.dumpStats(getInfo().toString());
	}

}
