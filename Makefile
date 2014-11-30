.PHONY: clean 

compile:
	rebar compile

run:
	(mkdir -p tmp)
	erl +K true +A30 -sname owm_resolver -pa ebin deps/*/ebin -config config/owm_resolver.config -eval '[application:start(A) || A <- [kernel, syntax_tools, compiler, goldrush,  lager, asn1, crypto, jsx, public_key, ssl, inets, ranch, cowlib, cowboy, sync, mnesia, owm_resolver] ]'

test: clean
	rebar eunit skip_deps=true

clean:
	(rm -rf tmp)
	rebar clean