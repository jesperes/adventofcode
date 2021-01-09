package aoc2020;

import java.io.File;
import java.io.FileNotFoundException;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Optional;

import aoc2020.Day22.Decks;
import common2.AocBaseRunner;
import common2.AocPuzzleInfo;
import common2.AocResult;
import common2.IAocLongPuzzle;

public class Day22 implements IAocLongPuzzle<Decks> {

    record Deck(List<Integer> cards) {
        /**
         * Return a new deck containing a copy of the list of cards passed in.
         */
        public static Deck getCopy(List<Integer> cards) {
            var list = new ArrayList<Integer>();
            list.addAll(cards);
            return new Deck(list);
        }

        long score() {
            long score = 0;
            for (int i = 0; i < cards.size(); i++) {
                score += cards.get(i) * (cards.size() - i);
            }
            return score;
        }

        public boolean isEmpty() {
            return cards.isEmpty();
        }

        public int drawFromTop() {
            return cards.remove(0);
        }

        public void insertAtBottom(int card) {
            cards.add(card);
        }

        public int size() {
            return cards.size();
        }
    }

    record Result(int player, long score) {
    }

    record Decks(Deck deck1, Deck deck2) {
    }

    @Override
    public AocPuzzleInfo getInfo() {
        return new AocPuzzleInfo(2020, 22, "Crab Combat", false);
    }

    @Override
    public AocResult<Long, Long> getExpected() {
        return AocResult.of(32677L, 33661L);
    }

    @Override
    public Decks parse(Optional<File> file) {
        return new Decks(
                new Deck(List.of(29, 21, 38, 30, 25, 7, 2, 36, 16, 44, 20, 12,
                        45, 4, 31, 34, 33, 42, 50, 14, 39, 37, 11, 43, 18)),
                new Deck(List.of(32, 24, 10, 41, 13, 3, 6, 5, 9, 8, 48, 49, 46,
                        17, 22, 35, 1, 19, 23, 28, 40, 26, 47, 15, 27)));
    }

    @Override
    public Long part1(Decks input) {
        Deck deck1 = Deck.getCopy(input.deck1.cards);
        Deck deck2 = Deck.getCopy(input.deck2.cards);

        while (true) {
            if (deck1.isEmpty())
                return deck2.score();
            else if (deck2.isEmpty())
                return deck1.score();

            int card1 = deck1.drawFromTop();
            int card2 = deck2.drawFromTop();

            if (card1 > card2) {
                deck1.insertAtBottom(card1);
                deck1.insertAtBottom(card2);
            } else if (card2 > card1) {
                deck2.insertAtBottom(card2);
                deck2.insertAtBottom(card1);
            } else {
                throw new RuntimeException();
            }
        }
    }

    @Override
    public Long part2(Decks input) {
        Deck deck1 = Deck.getCopy(input.deck1.cards);
        Deck deck2 = Deck.getCopy(input.deck2.cards);

        return combat(new Decks(deck1, deck2), new HashSet<Decks>()).score;
    }

    private Result combat(Decks decks, HashSet<Decks> seen) {
        final var deck1 = decks.deck1;
        final var deck2 = decks.deck2;

        while (true) {
            if (seen.contains(decks)) {
                // If we have seen this state before, player 1 wins.
                return new Result(1, decks.deck1.score());
            } else {
                if (deck1.isEmpty())
                    return new Result(2, decks.deck2.score());
                else if (deck2.isEmpty())
                    return new Result(1, decks.deck1.score());

                seen.add(decks);

                int card1 = deck1.drawFromTop();
                int card2 = deck2.drawFromTop();

                if (card1 <= deck1.size() && card2 <= deck2.size()) {

                    var subdeck1 = Deck.getCopy(deck1.cards.subList(0, card1));
                    var subdeck2 = Deck.getCopy(deck2.cards.subList(0, card2));

                    var result = combat(new Decks(subdeck1, subdeck2),
                            new HashSet<Decks>());
                    if (result.player == 1) {
                        deck1.insertAtBottom(card1);
                        deck1.insertAtBottom(card2);
                    } else {
                        deck2.insertAtBottom(card2);
                        deck2.insertAtBottom(card1);
                    }
                } else {
                    if (card1 > card2) {
                        deck1.insertAtBottom(card1);
                        deck1.insertAtBottom(card2);
                    } else if (card2 > card1) {
                        deck2.insertAtBottom(card2);
                        deck2.insertAtBottom(card1);
                    } else {
                        throw new RuntimeException();
                    }
                }
            }
        }
    }

    public static void main(String[] args) throws FileNotFoundException {
        AocBaseRunner.run(new Day22());
    }
}
