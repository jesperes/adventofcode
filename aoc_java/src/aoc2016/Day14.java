package aoc2016;

import static org.junit.Assert.assertEquals;

import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;

import org.junit.Test;

public class Day14 {
    static MessageDigest md5;

    static {
        try {
            md5 = MessageDigest.getInstance("MD5");
        } catch (NoSuchAlgorithmException e) {
            e.printStackTrace();
        }
    }

    String[] md5digests = new String[100000];

    private byte toHexByte(int halfbyte) {
        if (halfbyte <= 9) {
            return (byte) (halfbyte + '0');
        } else {
            return (byte) (halfbyte + 'a' - 10);
        }
    }

    private void toHexDigest(byte[] digest, byte[] hexdigest) {
        for (int i = 0; i < 16; i++) {
            byte b = digest[i];
            byte lower = (byte) (b & 0x0f);
            byte upper = (byte) ((b & 0xf0) >> 4);
            hexdigest[i * 2] = toHexByte(upper);
            hexdigest[i * 2 + 1] = toHexByte(lower);
        }
    }

    private String md5(String input, int key, boolean do_stretch) {
        if (md5digests[key] != null) {
            return md5digests[key];
        } else {
            String buf = input + String.valueOf(key);
            byte[] digest = md5.digest(buf.getBytes());
            byte[] s = new byte[32];

            if (!do_stretch) {
                toHexDigest(digest, s);
            } else {
                toHexDigest(digest, s);

                for (int i = 0; i < 2016; i++) {
                    toHexDigest(md5.digest(s), s);
                }
            }

            md5digests[key] = new String(s);
            return md5digests[key];
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

    private int findKey(String input, int startIdx, int keyTarget,
            boolean do_stretch) {
        int index = startIdx;
        int key = 0;

        while (true) {
            String s = md5(input, index, do_stretch);
            int c = hasSeq(s, 3);
            if (c == -1) {
                index++;
                continue;
            }

            for (int i = index + 1; i <= index + 1000; i++) {
                int c5 = hasSeq(md5(input, i, do_stretch), 5);
                if (c5 == c) {
                    // We have found a key
                    key++;

                    if (keyTarget == key) {
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
    public void testToHexDigest() {
        byte[] str = md5.digest("abc0".getBytes());
        byte[] digest = new byte[32];
        toHexDigest(str, digest);
        assertEquals("577571be4de9dcce85a041ba0410f29f", new String(digest));
    }

    @Test
    public void testMD5_part1() throws Exception {
        assertEquals("577571be4de9dcce85a041ba0410f29f", md5("abc", 0, false));
        assertEquals("0034e0923cc38887a57bd7b1d4f953df", md5("abc", 18, false));
    }

    @Test
    public void testMD5_part2() throws Exception {
        assertEquals("a107ff634856bb300138cac6568c0f24", md5("abc", 0, true));
    }

    @Test
    public void testPart1() throws Exception {
        assertEquals(23890, findKey("ahsbgdzn", 0, 64, false));
    }

    @Test
    public void testPart2() throws Exception {
        assertEquals(22696, findKey("ahsbgdzn", 0, 64, true));
    }
}
