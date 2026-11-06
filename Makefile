SCHEME ?= scheme
CAP ?=
OBJ := build/obj
LIBDIRS := src::$(OBJ)/src:tests/lib::$(OBJ)/tests/lib
RUN := $(SCHEME) --libdirs $(LIBDIRS) --script tests/run.ss
BUILD_TIMEOUT ?= 600
TEST_TIMEOUT ?= 300
TESTS := $(sort $(wildcard tests/*-test.sps))

.PHONY: build test bench clean $(TESTS:%=run/%)

# Phony so that Chez, which tracks what each object was built against,
# decides what is stale.
build:
	$(CAP) timeout $(BUILD_TIMEOUT) $(SCHEME) -q --script build.ss

# One process per test file, so a crash or leaked global state stays inside
# its file.
test: $(TESTS:%=run/%)

$(TESTS:%=run/%): run/%: build
	@$(CAP) timeout $(TEST_TIMEOUT) $(RUN) ./$(OBJ)/$(*:.sps=.so)

bench: build
	$(CAP) timeout $(TEST_TIMEOUT) $(RUN) ./$(OBJ)/bench/page.so

clean:
	rm -rf build
