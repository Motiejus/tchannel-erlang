all: compile lint dialyzer test proper cover

COVER = _build/test/cover/index.html
R3_URL = https://s3.amazonaws.com/rebar3/rebar3
REBAR3 = $(shell which rebar3 || echo ./rebar3)

compile:
	$(REBAR3) compile

dialyzer:
	$(REBAR3) as dialyzer dialyzer

test:
	$(REBAR3) eunit

proper:
	$(REBAR3) as test proper --cover

cover:
	$(REBAR3) cover -v
	@if [ -n "`sed -nE 's/[^0-9]*([0-9]+%).*/\1/p' $(COVER) | grep -v 100`" ]; then \
		echo Error: lack of coverage detected; exit 1; \
	else \
		echo "===> 100% code coverage. Keep it up!"; \
	fi

lint:
	$(REBAR3) as lint lint

clean:
	$(REBAR3) clean
	rm -fr _build/venv

setup: $(REBAR3)
	if [ ! -f _build/venv/bin/tcurl.py ]; then \
		mkdir -p _build; \
		virtualenv _build/venv; \
		_build/venv/bin/pip install tchannel==0.21.4; \
	fi

./rebar3:
	erl -noshell -s inets start -s ssl start \
        -eval 'httpc:request(get, {"$(R3_URL)",[]},[],[{stream,"./rebar3"}])' \
        -s inets stop -s init stop
	chmod +x ./rebar3

.PHONY: all compile dialyzer lint test setup cover
