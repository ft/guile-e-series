TOPDIR = .

LOAD_PATH = $(TOPDIR)/scheme
TEST_PATH = $(TOPDIR)/tests

GUILE_BINARY ?= guile
GUILE_CALL = $(GUILE_BINARY) -L $(LOAD_PATH) -C $(LOAD_PATH) --no-auto-compile
GUILD_BINARY ?= guild

CFLAGS  = -Wunsupported-warning -Wunused-variable # -Wunused-toplevel
CFLAGS += -Wunbound-variable -Warity-mismatch -Wduplicate-case-datum
CFLAGS += -Wbad-case-datum -Wformat -L$(LOAD_PATH)

COMPILE = $(GUILD_BINARY) compile $(CFLAGS)

TESTGUILE = ./tools/run-single-test
PROVE = tap-harness -e '$(TESTGUILE)'

INSTALL = $(GUILE_BINARY) --no-auto-compile ./tools/install
DESTDIR =
PREFIX = /usr/local

MODULES  = scheme/e-series.scm
MODULES += scheme/e-series/adjacency.scm
MODULES += scheme/e-series/exact.scm
MODULES += scheme/e-series/utilities.scm
MODULES += scheme/e-series/combine.scm
MODULES += scheme/e-series/tables.scm
MODULES += scheme/e-series/pretty-print.scm
MODULES += scheme/e-series/predicates.scm

OBJECTS = ${MODULES:.scm=.go}
TESTS = $(TEST_PATH)/*-scm.t

.SUFFIXES: .scm .go

all: $(OBJECTS)

.scm.go:
	$(COMPILE) -o $@ $<

test:
	$(PROVE) $(TESTS)

test-verbose:
	$(PROVE) --verbose $(TESTS)

install: all
	$(INSTALL) DESTDIR="$(DESTDIR)" PREFIX="$(PREFIX)"

clean:
	find . -name "*.go" -exec rm -f '{}' +
	find . -name "*~" -exec rm -f '{}' +

.PHONY: all clean install test
