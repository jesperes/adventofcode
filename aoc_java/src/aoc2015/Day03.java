package aoc2015;

import java.io.File;
import java.util.HashSet;
import java.util.Optional;
import java.util.Set;

import common2.AocPuzzleInfo;
import common2.AocResult;
import common2.IAocIntPuzzle;
import common2.InputUtils;

public class Day03 implements IAocIntPuzzle<byte[]> {

    record House(int x, int y) {
    }

    class Santa {
        private int x = 0, y = 0;
        private Set<House> houses;

        public Santa() {
            this.houses = new HashSet<>();
        }

        public Santa(Set<House> houses) {
            this.houses = houses;
        }

        void move(byte c) {
            switch (c) {
            case '<':
                x--;
                break;
            case '>':
                x++;
                break;
            case 'v':
                y++;
                break;
            case '^':
                y--;
                break;
            }
        }

        void deliver() {
            houses.add(new House(x, y));
        }

        int numHouses() {
            return houses.size();
        }
    }

    @Override
    public AocPuzzleInfo getInfo() {
        return new AocPuzzleInfo(2015, 3,
                "Perfectly Spherical Houses in a Vacuum", true);
    }

    @Override
    public AocResult<Integer, Integer> getExpected() {
        return AocResult.of(2572, 2631);
    }

    @Override
    public byte[] parse(Optional<File> file) {
        return InputUtils.asByteArray(file.get());
    }

    @Override
    public Integer part1(byte[] input) {
        Santa santa = new Santa();

        for (byte b : input) {
            santa.deliver();
            santa.move(b);
        }

        return santa.numHouses();
    }

    @Override
    public Integer part2(byte[] input) {
        Set<House> houses = new HashSet<>();
        Santa santa = new Santa(houses);
        Santa roboSanta = new Santa(houses);
        int i = 0;

        for (byte b : input) {
            santa.deliver();
            roboSanta.deliver();

            if (i % 2 == 0)
                santa.move(b);
            else
                roboSanta.move(b);
            i++;
        }

        return houses.size();
    }
}
