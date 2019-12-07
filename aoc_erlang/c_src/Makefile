CC=gcc
CFLAGS=-g -O3 -Wall -Werror -fPIC

all: aoc2016_day14.so

aoc2016_day14.o: aoc2016_day14.c
	$(CC) $(CFLAGS) -c $< -o $@

aoc2016_day14.so: aoc2016_day14.o
	$(CC) -shared -fPIC $^ -o $@

clean:
	rm -f *.so *.o
