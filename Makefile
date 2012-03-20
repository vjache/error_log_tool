REBAR=`which rebar || ./rebar`

all: clean compile

compile: deps
	@$(REBAR) compile

deps:
	@$(REBAR) get-deps

udeps:
	@$(REBAR) update-deps

test: force
	@$(REBAR) eunit

clean:
	@$(REBAR) clean

force: ;