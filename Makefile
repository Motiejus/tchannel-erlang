all: compile dialyzer test

compile:
	rebar3 compile

dialyzer:
	rebar3 dialyzer

test:
	rebar3 eunit

clean:
	rebar3 clean
	rm -fr _build/venv

setup:
	mkdir -p _build
	virtualenv _build/venv
	_build/venv/bin/pip install tchannel

.PHONY: all compile dialyzer test setup
