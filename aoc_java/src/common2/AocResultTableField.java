package common2;

public enum AocResultTableField {
    Year("Year"), //
    Day("Day"), //
    Name("Name"), //
    Parsing("Parsing"), // Time to parse
    Part1Time("Part 1"), // Time to solve part1
    Part2Time("Part 2"), // Time to solve part2
    TotalTime("Total"), //
    Part1Result("Part 1 result"), // Part 1 result
    Part2Result("Part 2 resutl"), // Part 2 result
    Part1Status("Part 1 status"), // Part 1 status
    Part2Status("Part 2 status"); // Part 2 status

    public String label;

    AocResultTableField(String string) {
        this.label = string;
    }
}
