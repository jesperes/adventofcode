package aoc2020;

import static org.junit.Assert.assertTrue;

import java.io.File;
import java.io.FileNotFoundException;
import java.util.Arrays;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Optional;
import java.util.Set;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import com.google.common.collect.Sets;

import aoc2020.Day21.Day21Data;
import common2.AocBaseRunner;
import common2.AocPuzzleInfo;
import common2.AocResult;
import common2.IAocPuzzle;
import common2.InputUtils;

public class Day21 implements IAocPuzzle<Day21Data, Long, String> {

    static Pattern p = Pattern
            .compile("(?<ingredients>.*) \\(contains (?<allergens>.*)\\)");

    /*
     * We wrap the ingredients in records, so that we can use types to
     * distinguish between ingredients and allergens (instead of both being
     * "string"). This makes for more readable code and better type checking.
     */
    record Ingredient(String name) {
    }

    record Allergen(String name) {
    }

    record Food(Set<Ingredient> ingredients, Set<Allergen> allergens) {
    }

    record Day21Data(List<Food> food,
            Map<Allergen, Set<Ingredient>> allergenMap) {
    }

    @Override
    public AocPuzzleInfo getInfo() {
        return new AocPuzzleInfo(2020, 21, "Allergen Assessment", true);
    }

    @Override
    public AocResult<Long, String> getExpected() {
        return AocResult.of(2428L,
                "bjq,jznhvh,klplr,dtvhzt,sbzd,tlgjzx,ctmbr,kqms");
    }

    @Override
    public Day21Data parse(Optional<File> file) {
        Stream<String> lines = Arrays
                .stream(InputUtils.asString(file.get()).split("\n"));

        List<Food> foods = lines.map(line -> p.matcher(line))
                .peek(matcher -> assertTrue(matcher.matches()))
                .map(matcher -> new Food(
                        Arrays.stream(matcher.group("ingredients").split(" "))
                                .map(name -> new Ingredient(name))
                                .collect(Collectors.toSet()),
                        Arrays.stream(matcher.group("allergens").split("[, ]+"))
                                .map(name -> new Allergen(name))
                                .collect(Collectors.toSet())))
                .collect(Collectors.toList());

        // Map of allergens to the set of ingredients that may contain it.
        Map<Allergen, Set<Ingredient>> allergenMap = new HashMap<>();
        for (Food food : foods) {
            for (Allergen allergen : food.allergens()) {
                Set<Ingredient> s1 = allergenMap.getOrDefault(allergen,
                        food.ingredients());
                Set<Ingredient> s2 = food.ingredients();
                allergenMap.put(allergen, Sets.intersection(s1, s2));
            }
        }

        return new Day21Data(foods, allergenMap);
    }

    @Override
    public Long part1(Day21Data input) {
        // Frequency map of all ingredients
        Map<Ingredient, Integer> freqMap = new HashMap<>();
        for (Food food : input.food) {
            for (Ingredient ingr : food.ingredients) {
                freqMap.compute(ingr, (k, v) -> (v == null) ? 1 : v + 1);
            }
        }

        Set<Ingredient> ingrWithAllergens = input.allergenMap.values().stream()
                .flatMap(set -> set.stream()).collect(Collectors.toSet());

        Set<Ingredient> allIngredients = input.food().stream()
                .flatMap(food -> food.ingredients.stream())
                .collect(Collectors.toSet());

        Set<Ingredient> ingrWithoutAllergens = Sets.difference(allIngredients,
                ingrWithAllergens);

        return ingrWithoutAllergens.stream()
                .collect(Collectors.summingLong(ingr -> freqMap.get(ingr)));
    }

    @Override
    public String part2(Day21Data input) {

        /*
         * allergens is a set view, so to modify it we need to make a copy of
         * it.
         */
        Map<Allergen, Set<Ingredient>> modifiableAllergenMap = new HashMap<>();
        for (Entry<Allergen, Set<Ingredient>> e : input.allergenMap
                .entrySet()) {
            var set = new HashSet<Ingredient>();
            set.addAll(e.getValue());
            modifiableAllergenMap.put(e.getKey(), set);
        }

        /*
         * Compute which ingredient contains which allergen.
         */
        Map<Ingredient, Allergen> map = new HashMap<>();
        while (map.size() < modifiableAllergenMap.size()) {
            // Find an allergen which can be uniquely matched to one ingredient
            Entry<Allergen, Set<Ingredient>> entry = modifiableAllergenMap
                    .entrySet().stream().filter(e -> {
                        Set<Ingredient> ingredients = e.getValue();
                        Allergen a = e.getKey();
                        return (ingredients.size() == 1
                                && !map.containsValue(a));
                    }).findAny().get();

            Allergen allergen = entry.getKey();
            Ingredient ingredient = entry.getValue().stream().findAny().get();

            // Store it in the map
            map.put(ingredient, allergen);

            // Remove it from all other allergens
            for (Allergen a : modifiableAllergenMap.keySet().stream()
                    .collect(Collectors.toList())) {
                modifiableAllergenMap.computeIfPresent(a, (k, v) -> {
                    if (a.name.equals(allergen.name)) {
                        return v;
                    } else {
                        v.remove(ingredient);
                        return v;
                    }
                });
            }
        }

        /*
         * The answer is the list of comma-separated string of ingredients,
         * sorted by their allergen.
         */
        return map.entrySet().stream()
                .sorted(new Comparator<Entry<?, Allergen>>() {
                    @Override
                    public int compare(Entry<?, Allergen> o1,
                            Entry<?, Allergen> o2) {
                        Allergen a1 = o1.getValue();
                        Allergen a2 = o2.getValue();
                        return a1.name.compareTo(a2.name);
                    }
                }).map(e -> {
                    Ingredient ingr = e.getKey();
                    return ingr.name;
                }).collect(Collectors.joining(","));
    }

    public static void main(String[] args) throws FileNotFoundException {
        AocBaseRunner.run(new Day21());
    }
}
