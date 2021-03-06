#!/usr/bin/awk -f

# Fuel required to launch a given module is based on its
# mass. Specifically, to find the fuel required for a module, take its
# mass, divide by three, round down, and subtract 2.

function fuel_part1(mass)
{
    return int(mass / 3) - 2
}

# For part 2, we need to add fuel for the fuel we just added, and so
# on. Fuel weighing zero or negative count as zero.

function fuel_part2(mass)
{
    f = fuel_part1(mass)
    if (f <= 0)
        return 0

    return f + fuel_part2(f)
}

{
    mass1 += fuel_part1($1)
    mass2 += fuel_part2($1)
}

END {
    exit (mass1 == 3368364 && mass2 == 5049684) ? 0 : 1
}
