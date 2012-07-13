all: almc

.PHONY: almc compile

almc:
	@rebar compile escriptize

compile:
	@rebar compile

test: almc
	@PATH=`pwd`:$$PATH rebar ct

clean:
	@rebar clean

very_clean: clean
	rm -rf logs
