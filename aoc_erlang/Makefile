ifeq (output-sync, $(filter output-sync, $(.FEATURES)))
MAKEFLAGS += --output-sync=none
endif

all:
	rebar3 do compile,dialyzer,eunit

clean:
	rebar3 clean
