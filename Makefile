
.PHONY: compiler emulator test

all: compiler emulator

compiler emulator test:
	$(MAKE) -C $@
