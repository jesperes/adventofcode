all: c java erlang

.PHONY: c java erlang
c:
	(cd aoc_c && \
		mkdir -p build && \
		cd build && \
		cmake .. && \
		make && \
		ctest)

java:
	(cd aoc_java && mvn test)

erlang:
	(cd aoc_erlang && rebar3 eunit)
