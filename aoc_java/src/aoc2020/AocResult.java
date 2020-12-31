package aoc2020;

import java.util.Optional;

public record AocResult<P1, P2> (P1 p1, P2 p2, Optional<AocTiming> timing) {
}
