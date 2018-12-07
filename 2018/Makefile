ERLC=erlc
ERLC_OPTIONS=-Werror

PUZZLE_SRC=$(wildcard */puzzle*.erl)
PUZZLE_OBJS=$(PUZZLE_SRC:%.erl=%.beam)

all: $(PUZZLE_OBJS)

%.beam: %.erl
	$(ERLC) $(ERLC_OPTIONS) -o $(@D) $<

run: all
	@for psrc in $(sort $(PUZZLE_SRC)); do \
		dir=$$(dirname "$$psrc") ;\
		echo "Running puzzle $$psrc..." ;\
		(cd $$dir; time erl -noshell \
			-eval "X = puzzle$$dir:start1(), erlang:display({day$$dir, part1, X}), erlang:halt().") ;\
		(cd $$dir; time erl -noshell \
			-eval "X = puzzle$$dir:start2(), erlang:display({day$$dir, part2, X}), erlang:halt().") ;\
	done

clean:
	rm -f $(PUZZLE_OBJS)
