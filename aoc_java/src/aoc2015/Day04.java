package aoc2015;

import java.io.File;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.Optional;

import common2.AocBaseRunner;
import common2.AocPuzzleInfo;
import common2.AocResult;
import common2.IAocIntPuzzle;

/**
 * The performance of this one is bound by the MD5 algorithm, so there is really
 * not much room for optimization.
 *
 * @author jesperes
 */
public class Day04 implements IAocIntPuzzle<String> {
    static private MessageDigest md5 = getMessageDigest();

    static private MessageDigest getMessageDigest() {
        try {
            return MessageDigest.getInstance("MD5");
        } catch (NoSuchAlgorithmException e) {
            throw new RuntimeException(e);
        }
    }

    @Override
    public AocPuzzleInfo getInfo() {
        return new AocPuzzleInfo(2015, 4, "The Ideal Stocking Stuffer", false);
    }

    @Override
    public AocResult<Integer, Integer> getExpected() {
        return AocResult.of(282749, 9962624);
    }

    @Override
    public String parse(Optional<File> file) {
        return "yzbqklnj";
    }

    @Override
    public Integer part1(String input) {
        StringBuilder s = new StringBuilder(input);
        int len = input.length();

        for (int i = 1;; i++) {
            s.setLength(len);
            s.append(Integer.toString(i));
            byte[] digest = md5.digest(s.toString().getBytes());
            if (digest[0] == 0 && digest[1] == 0 && (digest[2] & 0xf0) == 0) {
                return i;
            }
        }
    }

    @Override
    public Integer part2(String input) {
        StringBuilder s = new StringBuilder(input);
        int len = input.length();
        // Start at the part 1 solution
        for (int i = 282749;; i++) {
            s.setLength(len);
            s.append(Integer.toString(i));
            byte[] digest = md5.digest(s.toString().getBytes());
            if (digest[0] == 0 && digest[1] == 0 && digest[2] == 0) {
                return i;
            }
        }
    }

    public static void main(String[] args) {
        AocBaseRunner.run(new Day04());
    }
}
