all: aoc2015-java aoc2015-erlang aoc2018-erlang

aoc2015-java:
	@echo "Aoc 2015 solutions (Java)"
	@echo "==========================================="
	@(cd aoc-java && time -p ant) >/dev/null

aoc2015-erlang:
	@echo "Aoc 2015 solutions (Erlang)"
	@echo "==========================================="
	@(cd 2015 && time -p escript ./aoc2015.escript) >/dev/null

aoc2018-erlang:
	@echo "Aoc 2018 solutions (Erlang)"
	@echo "==========================================="
	@(cd 2018 && time -p escript ./aoc2018.escript) >/dev/null
