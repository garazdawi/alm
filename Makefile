
.PHONY: compiler emulator test

all: compiler emulator

compiler: bin
	@$(MAKE) -C $@
	@cp $@/almc bin/

emulator: bin
	@$(MAKE) -C $@
	@cp $@/alm bin/

test: compiler emulator
	$(MAKE) -C $@

bin:
	mkdir -p $@