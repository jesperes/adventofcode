package aoc2015;

import static org.junit.Assert.assertEquals;

import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;

import org.junit.Test;

/**
 * The performance of this one is bound by the MD5 algorithm, so there is really
 * not much room for optimization.
 *
 * @author jesperes
 */
public class Day04 {
    static private final String INPUT = "yzbqklnj";
    private MessageDigest md5;

    public Day04() throws NoSuchAlgorithmException {
        md5 = MessageDigest.getInstance("MD5");
    }

    boolean isPart1Solution(byte[] digest) {
        return digest[0] == 0 && digest[1] == 0 && (digest[2] & 0xf0) == 0;
    }

    boolean isPart2Solution(byte[] digest) {
        return digest[0] == 0 && digest[1] == 0 && digest[2] == 0;
    }

    @Test
    public void testDay04() throws Exception {
        int p1 = 0;
        int p2 = 0;
        StringBuilder s = new StringBuilder(INPUT);

        for (int i = 1;; i++) {
            s.setLength(INPUT.length());
            s.append(String.valueOf(i));

            byte[] digest = md5.digest(s.toString().getBytes());

            if (p1 == 0 && isPart1Solution(digest)) {
                p1 = i;
            }
            if (isPart2Solution(digest)) {
                p2 = i;
                break;
            }

        }

        assertEquals(282749, p1);
        assertEquals(9962624, p2);
    }
}
