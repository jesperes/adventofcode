import java.util.HashMap;
import java.util.Map;
import java.util.Optional;
import java.util.concurrent.TimeUnit;

public class Puzzle22 {

    public static void main(String[] args) {
        solve(36 * 1000 * 1000);
        // solve(100);
    }

    /**
     * Deliver all the presents for the given elf, depositing the number of
     * presents in the given map. Returns an optional containing the solution,
     * if found.
     */
    private static Optional<Integer> deliver(int elf,
            Map<Integer, Integer> houses, int limit) {
        int p = elf * 11;
        // System.out.println("Elf: " + elf);

        for (int house = elf; house <= elf * 50; house += elf) {
            houses.compute(house, (k, v) -> v == null ? p : v + p);
        }

        int q = houses.get(elf);
        if (q >= limit) {
            return Optional.of(elf);
        } else {
            houses.remove(elf);
            return Optional.empty();
        }
    }

    private static void solve(int limit) {
        long t0 = System.nanoTime();
        Map<Integer, Integer> houses = new HashMap<>();

        for (int elf = 1; true; elf++) {
            Optional<Integer> result = deliver(elf, houses, limit);
            if (result.isPresent()) {
                long elapsed = System.nanoTime() - t0;
                System.out.format("Result: %s (%s ms)%n", result.get(),
                        TimeUnit.NANOSECONDS.toMillis(elapsed));
                break;
            }
        }
    }
}
