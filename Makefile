.PHONY: clean 

compile:
	rebar compile

run:
	(mkdir -p tmp)
	erl +K true +A30 -sname openweather_resolver -pa ebin deps/*/ebin -config config/openweather_resolver.config -eval '[application:start(A) || A <- [kernel, syntax_tools, compiler, goldrush,  lager, asn1, crypto, jsx, public_key, ssl, inets, ranch, cowlib, cowboy, sync, mnesia, openweather_resolver] ]'

test: clean
	rebar eunit skip_deps=true

clean:
	(rm -rf tmp)
	rebar clean