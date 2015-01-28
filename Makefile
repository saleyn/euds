all : compile

.PHONY: compile release clean test
compile:
	rebar compile

test:
	rebar eunit

clean:
	rebar clean
