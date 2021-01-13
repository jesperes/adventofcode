package aoc2015;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Optional;
import java.util.function.Function;
import java.util.stream.Collectors;

import aoc2015.Day15.Ingredient;
import common2.AocBaseRunner;
import common2.AocPuzzleInfo;
import common2.AocResult;
import common2.IAocIntPuzzle;
import common2.InputUtils;

public class Day15 implements IAocIntPuzzle<List<Ingredient>> {

    record Ingredient(String name, int capacity, int durability, int flavor,
            int texture, int calories) {
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

    private int getProperty(List<Integer> combos, List<Ingredient> ingredients,
            Function<Ingredient, Integer> fun) {
        int propsum = 0;
        for (int i = 0; i < combos.size(); i++) {
            propsum += combos.get(i) * fun.apply(ingredients.get(i));
        }
        return Math.max(0, propsum);
    }

    private int score(List<Ingredient> ingredients, List<Integer> combo) {
        return getProperty(combo, ingredients, ingr -> ingr.capacity)
                * getProperty(combo, ingredients, ingr -> ingr.durability)
                * getProperty(combo, ingredients, ingr -> ingr.flavor)
                * getProperty(combo, ingredients, ingr -> ingr.texture);
    }

    @Override
    public AocPuzzleInfo getInfo() {
        return new AocPuzzleInfo(2015, 15, "Science for Hungry People", true);
    }

    @Override
    public AocResult<Integer, Integer> getExpected() {
        return AocResult.of(222870, 117936);
    }

    @Override
    public List<Ingredient> parse(Optional<File> file) throws IOException {
        return InputUtils.asStringList(file.get()).stream().map(line -> {
            String[] s = line.split("[ :,]");
            String name = s[0];
            int capacity = Integer.valueOf(s[3]);
            int durability = Integer.valueOf(s[6]);
            int flavor = Integer.valueOf(s[9]);
            int texture = Integer.valueOf(s[12]);
            int calories = Integer.valueOf(s[15]);
            return new Ingredient(name, capacity, durability, flavor, texture,
                    calories);
        }).collect(Collectors.toUnmodifiableList());
    }

    @Override
    public Integer part1(List<Ingredient> ingredients) {
        return get4IngredientCombos().stream()
                .mapToInt(combo -> score(ingredients, combo))
                .summaryStatistics().getMax();
    }

    @Override
    public Integer part2(List<Ingredient> ingredients) {
        return get4IngredientCombos().stream()
                .filter(combo -> getProperty(combo, ingredients,
                        ingr -> ingr.calories) == 500)
                .mapToInt(combo -> score(ingredients, combo))
                .summaryStatistics().getMax();
    }

    public static void main(String[] args) {
        AocBaseRunner.run(new Day15());
    }
}
