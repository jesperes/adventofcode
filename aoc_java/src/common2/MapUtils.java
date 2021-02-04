package common2;

import java.util.Comparator;
import java.util.Map.Entry;

public class MapUtils {

    /**
     * Create a comparator for comparing map entries by values. This can be used
     * e.g. to find the entry in a map with the largest value.
     * 
     * @param <K>
     * @param <V>
     * @return
     */
    public static <K, V extends Comparable<V>> Comparator<Entry<K, V>> mapValueComparator() {
        return new Comparator<Entry<K, V>>() {
            @Override
            public int compare(Entry<K, V> o1, Entry<K, V> o2) {
                return o1.getValue().compareTo(o2.getValue());
            }
        };
    }
}
