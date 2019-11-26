package common;

public class Pair<T0, T1> {
    public final T0 x;
    public final T1 y;

    public Pair(T0 first, T1 second) {
        this.x = first;
        this.y = second;
    }

    @Override
    public int hashCode() {
        return x.hashCode() ^ y.hashCode();
    }

    @Override
    public boolean equals(Object obj) {
        @SuppressWarnings("unchecked")
        Pair<T0, T1> other = (Pair<T0, T1>) obj;
        return other.x.equals(x) && other.y.equals(y);
    }

    @Override
    public String toString() {
        return String.format("{%s,%s}", x, y);
    }

}
