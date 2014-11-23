ERL = $(shell which erl)

ERLFLAGS= -pa $(CURDIR)/.eunit -pa $(CURDIR)/ebin -pa $(CURDIR)/*/ebin

PROJ_PLT=$(CURDIR)/.depsolver_plt

.PHONY: dialyzer typer clean distclean

compile:
	rebar compile
run:
	erl +K true +A30 -sname openweather_resolver -pa ebin deps/*/ebin -eval '[application:start(A) || A <- [kernel, syntax_tools, compiler, goldrush,  lager, asn1, crypto, jsx, public_key, ssl, inets, ranch, cowlib, cowboy, ibrowse, sync, openweather_resolver] ]'

$(PROJ_PLT):
	dialyzer --output_plt $(PROJ_PLT) --build_plt \
		--apps erts kernel stdlib crypto public_key -r deps --fullpath

dialyzer: $(PROJ_PLT)
	dialyzer --plt $(PROJ_PLT) -pa deps/* --src src

typer: $(PROJ_PLT)
	typer --plt $(PROJ_PLT) -r ./src

clean:
	$(REBAR) clean

distclean: clean
	rm $(PROJ_PLT)
	rm -rvf $(CURDIR)/deps/*
