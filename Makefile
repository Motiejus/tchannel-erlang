all: compile lint dialyzer test cover

compile:
	rebar3 compile

dialyzer:
	rebar3 dialyzer

test:
	rebar3 eunit

cover:
	rebar3 cover -v

lint:
	rebar3 as lint lint

clean:
	rebar3 clean
	rm -fr _build/venv

setup:
	mkdir -p _build
	virtualenv _build/venv
	_build/venv/bin/pip install tchannel

.PHONY: all compile dialyzer lint test setup cover
