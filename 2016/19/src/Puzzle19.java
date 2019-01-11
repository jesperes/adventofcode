import static org.junit.Assert.assertEquals;

import org.junit.Test;

public class Puzzle19 {
    static class Elf {
        final int num;
        int presents = 1;
        Elf next = null;
        Elf prev = null;

        Elf(int n) {
            num = n;
            next = this;
            prev = this;
        }

        void insertAfter(Elf elf) {
            elf.next = next;
            elf.prev = this;
            next.prev = elf;
            next = elf;
        }

        void remove() {
            prev.next = next;
            next.prev = prev;
        }

        void steal(Elf elf) {
            presents += elf.presents;
            elf.remove();
        }

        boolean isAlone() {
            return next == this;
        }

        @Override
        public String toString() {
            return "Elf [num=" + num + ", presents=" + presents + "]";
        }
    }

    static int run_part1(int elves) {
        Elf curr = create_table(elves);

        while (!curr.isAlone()) {
            curr.steal(curr.next);
            curr = curr.next;
        }

        return curr.num;
    }

    static int run_part2(int elves) {
        Elf curr = create_table(elves);
        Elf across = curr;
        int half = elves / 2;
        for (int i = 0; i < half; i++) {
            across = across.next;
        }

        while (!curr.isAlone()) {
            curr.steal(across);
            curr = curr.next;

            // update the across ref
            across = across.next;
            if (elves % 2 == 1)
                across = across.next;
            elves--;
        }

        return curr.num;
    }

    private static Elf create_table(int num) {
        Elf curr = new Elf(1);
        Elf first = curr;
        for (int i = 2; i <= num; i++) {
            curr.insertAfter(new Elf(i));
            curr = curr.next;
        }
        return first;
    }

    @Test
    public void testPart1() throws Exception {
        assertEquals(3, run_part1(5));
        assertEquals(1834471, run_part1(3014387));
        assertEquals(2, run_part2(5));
        assertEquals(1420064, run_part2(3014387));
    }
}
