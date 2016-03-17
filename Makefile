all: compile lint dialyzer test cover

COVER = _build/test/cover/index.html
R3_URL = https://s3.amazonaws.com/rebar3/rebar3
REBAR3 = $(shell which rebar3 || echo ./rebar3)

compile:
	$(REBAR3) compile

dialyzer:
	$(REBAR3) as test dialyzer

test:
	$(REBAR3) as test eunit

cover:
	$(REBAR3) as test cover -v
	@if [ -n "`sed -nE 's/[^0-9]*([0-9]+%).*/\1/p' $(COVER) | grep -v 100`" ]; then \
		echo Error: lack of coverage detected; exit 1; \
	fi

lint:
	$(REBAR3) as test lint

clean:
	$(REBAR3) clean
	rm -fr _build/venv

setup: $(REBAR3)
	mkdir -p _build
	virtualenv _build/venv
	_build/venv/bin/pip install tchannel==0.21.4

./rebar3:
	erl -noshell -s inets start -s ssl start \
        -eval 'httpc:request(get, {"$(R3_URL)",[]},[],[{stream,"./rebar3"}])' \
        -s inets stop -s init stop
	chmod +x ./rebar3

.PHONY: all compile dialyzer lint test setup cover
