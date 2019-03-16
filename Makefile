all: aoc2015-java aoc2015-erlang aoc2018-erlang

aoc2015-java:
	@echo "Aoc 2015 solutions (Java)"
	@echo "==========================================="
	@(cd aoc-java && time ant)

aoc2015-erlang:
	@echo "Aoc 2015 solutions (Erlang)"
	@echo "==========================================="
	@(cd 2015 && time escript ./aoc2015.escript)

aoc2018-erlang:
	@echo "Aoc 2018 solutions (Erlang)"
	@echo "==========================================="
	@(cd 2018 && time escript ./aoc2018.escript)
