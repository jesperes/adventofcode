package common;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;

public class Combinatorics {
    public static <T> List<List<T>> permutations(Collection<T> list) {
        List<List<T>> perms = new ArrayList<List<T>>();

        if (list.size() == 0) {
            perms.add(Collections.emptyList());
        } else {
            for (T elem : list) {
                List<T> rest = new ArrayList<>();
                rest.addAll(list);
                rest.remove(elem);

                for (List<T> subperm : permutations(rest)) {
                    List<T> perm = new ArrayList<>();
                    perm.add(elem);
                    perm.addAll(subperm);
                    perms.add(perm);
                }
            }
        }
        return perms;
    }
}
