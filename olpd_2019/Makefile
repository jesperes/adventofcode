ALL=\
	day01 \
	day02 \
	day04

all: $(ALL)
.PHONY: $(ALL)
.SILENT:

PROGRESS=@echo -n "$@... "
TIME=time --format='OK (elapsed time: %E)'

day01:
	$(PROGRESS)
	cd day01 && $(TIME) awk -f day01.awk < input.txt

day02:
	$(PROGRESS)
	cd day02 && $(TIME) groovy day02.groovy 2>&1 | grep -v "^WARNING:"

day04:
	$(PROGRESS)
	gcc -Wall -Werror -Wextra -O3 day04/day04.c -o day04/day04 && $(TIME) ./day04/day04
