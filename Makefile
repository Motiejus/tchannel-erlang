all: compile lint dialyzer test cover

COVER := _build/test/cover/index.html

compile:
	rebar3 compile

dialyzer:
	rebar3 dialyzer

test:
	rebar3 eunit

cover:
	rebar3 cover -v
	@if [ -n "`sed -nE 's/[^0-9]*([0-9]+%).*/\1/p' $(COVER) | grep -v 100`" ]; then \
		echo Error: lack of coverage detected; exit 1; \
	fi

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
