package common2;

public record AocPuzzleInfo(int year, int day, String name,
		boolean hasInputFile) {
	public String toString() {
		return String.format("%d day %d (%s)", year, day, name);
	}
}
