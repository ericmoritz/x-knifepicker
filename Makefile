all:
	rebar compile

deps:
	rebar get-deps

clean:
	rm *.pyc *.beam

test:
	rebar eunit skip_deps=true
