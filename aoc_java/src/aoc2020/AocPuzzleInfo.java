package aoc2020;

public class AocPuzzleInfo {
	public final int year;
	public final int day;
	public final String name;
	public boolean hasInputFile = true;

	public AocPuzzleInfo(int year, int day, String name) {
		this.year = year;
		this.day = day;
		this.name = name;
	}

	public AocPuzzleInfo(int year, int day, String name, boolean hasInputFile) {
		this.year = year;
		this.day = day;
		this.name = name;
		this.hasInputFile = hasInputFile;
	}
}
