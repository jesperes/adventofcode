CC=gcc
CFLAGS=-g -O3 -Wall -Werror -fPIC

# Get includes right in TravisCI
ifneq ($(ERLANG_ROOT_DIR),)
ERL_INC=-I$(ERLANG_ROOT_DIR)/usr/include
endif

all: aoc2016_day14.so

aoc2016_day14.o: aoc2016_day14.c
	$(CC) $(CFLAGS) $(ERL_INC) -c $< -o $@

aoc2016_day14.so: aoc2016_day14.o
	$(CC) -shared -fPIC $^ -o $@

clean:
	rm -f *.so *.o
