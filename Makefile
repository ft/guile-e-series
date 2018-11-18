RUNTESTS = run-tests -strip-roots -dispatch-root "$$PWD/tests"

BYTECOMPILE = sh ./tools/compile
INSTALL = sh ./tools/install

all:
	@echo
	@echo " Available targets:"
	@echo
	@echo "           all: This help text"
	@echo "       compile: Byte-compile the scheme library"
	@echo "         clean: Remove byte-compiled scheme code"
	@echo "       install: Install the project to the host system"
	@echo "          test: Run test suite"
	@echo "  test-verbose: Run test suite (with verbose test harness)"
	@echo "    test-debug: Run test suite (With all debugging enabled)"
	@echo

compile:
	$(BYTECOMPILE)

clean:
	find scheme -name '*.go' -exec rm '{}' +

install:
	$(INSTALL)

test:
	$(RUNTESTS)

test-verbose:
	$(RUNTESTS) -verbose

test-debug:
	$(RUNTESTS) -verbose -dispatch-verbose -debug

.PHONY: all compile clean install test test-debug test-verbose
