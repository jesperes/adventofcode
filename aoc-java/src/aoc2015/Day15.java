package aoc2015;

import java.io.BufferedReader;
import java.io.FileReader;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.junit.Test;

public class Day15 {
    class Ingredient {
        String name;
        int capacity;
        int durability;
        int flavor;
        int texture;
        int calories;

        public Ingredient(String name, int capacity, int durability, int flavor,
                int texture, int calories) {
            this.name = name;
            this.capacity = capacity;
            this.durability = durability;
            this.flavor = flavor;
            this.texture = texture;
            this.calories = calories;
        }
    }

    List<List<Integer>> get4IngredientCombos() {
        List<List<Integer>> combos = new ArrayList<>();

        for (int i1 = 0; i1 <= 100; i1++) {
            for (int i2 = 0; i2 <= 100; i2++) {
                for (int i3 = 0; i3 <= 100; i3++) {
                    for (int i4 = 0; i4 <= 100; i4++) {
                        if (i1 + i2 + i3 + i4 == 100) {
                            combos.add(Arrays.asList(i1, i2, i3, i4));
                        }
                    }
                }
            }
        }

        return combos;
    }

    @Test
    public void testDay15() throws Exception {

        List<Ingredient> ingredients = new ArrayList<>();

        try (BufferedReader reader = new BufferedReader(
                new FileReader("inputs/2015/day15.txt"))) {
            String line;

            while ((line = reader.readLine()) != null) {
                String[] s = line.split("[ :,]");

                for (int i = 0; i < s.length; i++) {
                    System.out.format("s[%d] = %s\n", i, s[i]);
                }

                String name = s[0];
                int capacity = Integer.valueOf(s[3]);
                int durability = Integer.valueOf(s[6]);
                int flavor = Integer.valueOf(s[9]);
                int texture = Integer.valueOf(s[12]);
                int calories = Integer.valueOf(s[15]);
                Ingredient ingr = new Ingredient(name, capacity, durability,
                        flavor, texture, calories);
                ingredients.add(ingr);
            }
        }
    }
}
