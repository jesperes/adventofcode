package aoc2015;

import static org.junit.Assert.assertEquals;

import java.io.BufferedReader;
import java.io.FileReader;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.function.Function;

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
                if (i1 + i2 > 100)
                    continue;

                for (int i3 = 0; i3 <= 100; i3++) {
                    if (i1 + i2 + i3 > 100)
                        continue;

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

    int getProperty(List<Integer> combos, List<Ingredient> ingredients,
            Function<Ingredient, Integer> fun) {
        int propsum = 0;
        for (int i = 0; i < combos.size(); i++) {
            propsum += combos.get(i) * fun.apply(ingredients.get(i));
        }
        return Math.max(0, propsum);
    }

    @Test
    public void testDay15() throws Exception {

        List<Ingredient> ingredients = new ArrayList<>();

        try (BufferedReader reader = new BufferedReader(
                new FileReader("inputs/2015/day15.txt"))) {
            String line;

            while ((line = reader.readLine()) != null) {
                String[] s = line.split("[ :,]");
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

        int max = Integer.MIN_VALUE;
        int maxCal500 = Integer.MIN_VALUE;

        for (List<Integer> combo : get4IngredientCombos()) {

            int capacity = getProperty(combo, ingredients,
                    ingr -> ingr.capacity);
            int durability = getProperty(combo, ingredients,
                    ingr -> ingr.durability);
            int flavor = getProperty(combo, ingredients, ingr -> ingr.flavor);
            int texture = getProperty(combo, ingredients, ingr -> ingr.texture);
            int calories = getProperty(combo, ingredients,
                    ingr -> ingr.calories);

            int score = capacity * durability * flavor * texture;
            if (score > max)
                max = score;

            if (calories == 500 && score > maxCal500) {
                maxCal500 = score;
            }
        }

        assertEquals(222870, max);
        assertEquals(117936, maxCal500);
    }
}
