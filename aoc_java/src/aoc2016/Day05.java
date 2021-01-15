package aoc2016;

import java.io.File;
import java.io.IOException;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.HashMap;
import java.util.Map;
import java.util.Optional;

import common2.AocBaseRunner;
import common2.AocPuzzleInfo;
import common2.AocResult;
import common2.IAocPuzzle;

public class Day05 implements IAocPuzzle<String, String, String> {

    static private final MessageDigest MD5 = getMD5();

    @Override
    public AocPuzzleInfo getInfo() {
        return new AocPuzzleInfo(2016, 5, "How About a Nice Game of Chess?",
                false);
    }

    private static MessageDigest getMD5() {
        try {
            return MessageDigest.getInstance("MD5");
        } catch (NoSuchAlgorithmException e) {
            throw new RuntimeException(e);
        }
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

    private static Map<Integer, byte[]> md5cache = new HashMap<>();

    private boolean hasLeadingZeroes(String input, int n, byte[] hexdigest) {

        if (md5cache.containsKey(n)) {
            byte[] cachedHexDigest = md5cache.get(n);
            System.arraycopy(cachedHexDigest, 0, hexdigest, 0,
                    cachedHexDigest.length);
        } else {
            String s = input + Integer.toString(n);
            byte[] digest = MD5.digest(s.getBytes());
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
}
