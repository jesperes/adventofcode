package common2;

import java.util.Optional;

public record AocResult<P1, P2> (Optional<P1> p1, Optional<P2> p2,
        Optional<AocTiming> timing) {
    public static <P1, P2> AocResult<P1, P2> of(P1 p1, P2 p2) {
        return new AocResult<P1, P2>(
                (p1 == null) ? Optional.empty() : Optional.of(p1),
                (p2 == null) ? Optional.empty() : Optional.of(p2),
                Optional.empty());
    }

    public static <P1, P2> AocResult<P1, P2> of(P1 p1, P2 p2,
            AocTiming timing) {
        return new AocResult<P1, P2>(
                (p1 == null) ? Optional.empty() : Optional.of(p1),
                (p2 == null) ? Optional.empty() : Optional.of(p2),
                Optional.of(timing));
    }
}
