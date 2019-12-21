ALL=\
	day01-awk \
	day01-c \
	day02-groovy \
	day04-c \
	day04-clpfd \
	day09-c

all: $(ALL)
.PHONY: $(ALL)
.SILENT:

PROGRESS=@printf "%-20s > " $@
TIME=time --format='OK (elapsed time: %E)'

day01-awk:
	$(PROGRESS)
	cd day01 && $(TIME) awk -f day01.awk < input.txt

day01-c:
	$(PROGRESS)
	gcc -Wall -Werror -Wextra -O3 day01/day01.c -o day01/day01 && \
		$(TIME) ./day01/day01 < day01/input.txt

day02-groovy:
	$(PROGRESS)
	cd day02 && $(TIME) groovy day02.groovy 2>&1 | grep -v "^WARNING:"

day04-c:
	$(PROGRESS)
	gcc -Wall -Werror -Wextra -O3 day04/day04.c -o day04/day04 && \
		$(TIME) ./day04/day04

day04-clpfd:
	$(PROGRESS)
	cd day04 && $(TIME) swipl day04.pl

day09-c:
	$(PROGRESS)
	gcc -c -Wall -Werror -O3 day09/intcode.c -o day09/intcode.o
	gcc -c -Wall -Werror -O3 day09/day09.c -o day09/day09.o
	gcc -Wall -Werror -O3 day09/intcode.o day09/day09.o -o day09/day09
	$(TIME) ./day09/day09
