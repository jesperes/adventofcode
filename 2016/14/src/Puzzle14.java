import static org.junit.Assert.assertEquals;

import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.HashMap;
import java.util.Map;

import javax.xml.bind.DatatypeConverter;

import org.junit.Test;

public class Puzzle14 {
    static MessageDigest md5;

    static {
        try {
            md5 = MessageDigest.getInstance("MD5");
        } catch (NoSuchAlgorithmException e) {
            e.printStackTrace();
        }
    }

    static Map<String, String> md5sums = new HashMap<>();

    private static String md5(String input, int key, int repeat) {
        if (repeat == 0) {
            String buf = input + String.valueOf(key);
            if (!md5sums.containsKey(buf)) {
                byte[] digest = md5.digest(buf.getBytes());
                String s = DatatypeConverter.printHexBinary(digest)
                        .toLowerCase();
                md5sums.put(buf, s);
            }
            return md5sums.get(buf);
        } else {
            String buf = input + String.valueOf(key);
            byte[] digest = md5.digest(buf.getBytes());
            buf = DatatypeConverter.printHexBinary(digest).toLowerCase();

            for (int i = 0; i < repeat; i++) {
                digest = md5.digest(buf.getBytes());
                buf = DatatypeConverter.printHexBinary(digest).toLowerCase();
            }

            return buf;
        }
    }

    static private int hasSeq(String s, int len) {
        int l = len;
        int c = 0;
        for (int c0 : s.toCharArray()) {
            if (c0 == c) {
                l--;
                if (l == 0)
                    return c;
            } else {
                l = len - 1;
                c = c0;
            }
        }

        return -1;
    }

    private static int findKey(String input, int startIdx, int keyTarget,
            int repeat) {
        int index = startIdx;
        int key = 0;

        while (true) {
            System.out.format("Checking index: %d%n", index);
            String s = md5(input, index, repeat);
            int c = hasSeq(s, 3);
            if (c == -1) {
                index++;
                continue;
            }

            for (int i = index + 1; i <= index + 1000; i++) {
                int c5 = hasSeq(md5(input, i, repeat), 5);
                if (c5 == c) {
                    // We have found a key
                    key++;

                    if (keyTarget == key) {
                        System.out.format("Found final key #%d at index %d%n",
                                key, index);
                        return index;
                    }
                }
            }

            index++;
        }
    }

    @Test
    public void testHasSeq() throws Exception {
        assertEquals('c', hasSeq("abccd", 2));
        assertEquals(-1, hasSeq("abcabcabc", 3));
        assertEquals(-1, hasSeq("abccd", 3));
        assertEquals('c', hasSeq("abcccd", 2));
        assertEquals('c', hasSeq("abcccd", 3));
    }

    @Test
    public void testFindKey() throws Exception {
        assertEquals(22728, findKey("abc", 0, 64, 0));
        assertEquals(23890, findKey("ahsbgdzn", 0, 64, 0));
        assertEquals(22696, findKey("ahsbgdzn", 0, 64, 2016));
    }

    @Test
    public void testMD5() throws Exception {
        assertEquals("0034e0923cc38887a57bd7b1d4f953df", md5("abc", 18, 0));
        assertEquals("577571be4de9dcce85a041ba0410f29f", md5("abc", 0, 0));
        assertEquals("a107ff634856bb300138cac6568c0f24", md5("abc", 0, 2016));
    }
}
