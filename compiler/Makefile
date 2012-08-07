all: almc

.PHONY: almc compile

almc: get-deps
	@rebar compile escriptize

compile: get-deps
	@rebar compile

get-deps:
	@rebar get-deps

test: almc
	@PATH=`pwd`:$$PATH rebar ct skip_deps=true

clean:
	@rebar clean

very_clean: clean
	rm -rf logs
