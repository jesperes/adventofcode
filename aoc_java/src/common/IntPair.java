package common;

public class IntPair extends Pair<Integer, Integer> {
    public IntPair(Integer first, Integer second) {
        super(first, second);
    }

    public static IntPair pair(int x, int y) {
        return new IntPair(x, y);
    }

    public IntPair copy() {
        return new IntPair(x, y);
    }
}
