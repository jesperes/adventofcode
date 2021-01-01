# Advent of Code 2020 - A comparison of Java, Erlang, Python

## [Day 1 - Report Repair](https://adventofcode.com/2020/day/1)

### Part 1

The first puzzle is all about finding pairs (and later triplets) of
integers matching a certain condition (`x + y == 2020`). The input is
a list of integers.

[List
comprehensions](https://en.wikipedia.org/wiki/List_comprehension) make
it very straight forward in Erlang and Python:

```erlang
solve1(Input) ->
  hd([X * Y || X <- Input,
               Y <- Input,
               X + Y == 2020]).
```

```python
def solve1(input):
  return [x * y
          for x in input
          for y in input
          if (x + y == 2020)
         ][0]
```

In Java we have to resort to a plain nested loop, which is simple enough:

```java
for (long x : input) {
    for (long y : input) {
        if (x + y == 2020) {
            return x * y;
        }
    }
}
```

### Part 2

In part 2 we are asked to find _triplets_, `{x, y, z}` which satisfy
the condition `x + y + z == 2020`. The solutions are easily modified
to do this.

### Improvements

The Erlang and Python list comprehensions will check all combinations
of `{x, y}` which is not really necessary since we only want to find
one (the Java solution exits immediately). One improvement would then
be to try to improve on this, although the size of the inputs is
probably too small for that effort to pay off.

In part 2, however, the additional dimension causes significantly
increased runtimes. The naive Python solution goes from ~3ms to more
than 500ms. We can improve on this by noting that for some pairs x +
y there is no z which can match:

```python
    def solve2(self, data):
        minz = min(data)
        return [x * y * z
                for x in data
                for y in data
                if (x + y) < 2020 - minz
                for z in data
                if (x + y + z == 2020)
            ][0]
```

We add an additional condition to the list comprehension to check that
for each (x + y) pair, there is "room" for a z value. The effect of
this optimization will depend on the distribution of values. In my
input, the values were highly skewed upwards (there were lots of
numbers > 1700) so the `if (x + y < 2020)` condition is true for a
large amount of x + y pairs.

### Timing

Not much to say about timing here; all three languages perform
similarly under ~10ms.
