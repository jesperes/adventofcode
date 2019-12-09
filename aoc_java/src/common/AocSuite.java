package common;

import org.junit.runner.RunWith;
import org.junit.runners.Suite;
import org.junit.runners.Suite.SuiteClasses;

import aoc2015.Aoc2015Suite;
import aoc2016.Aoc2016Suite;
import aoc2017.Aoc2017Suite;
import aoc2018.Aoc2018Suite;
import aoc2019.Aoc2019Suite;

@RunWith(Suite.class)
@SuiteClasses({ Aoc2015Suite.class, Aoc2016Suite.class, Aoc2017Suite.class,
        Aoc2018Suite.class, Aoc2019Suite.class })
public class AocSuite {
}
