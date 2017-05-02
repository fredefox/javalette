all: build

PROJECT_ROOT = $(shell stack path --project-root)
LIB_DIR      = $(PROJECT_ROOT)/lib
DIST_DIR     = $(PROJECT_ROOT)/$(shell stack path --dist-dir)
JLC_DIR      = $(DIST_DIR)/build/jlc
JLC_EXE      = $(JLC_DIR)/jlc
TARBALL      = $(DIST_DIR)/$(shell stack list-dependencies | grep javalette | sed  's/ /-/').tar.gz

build:
	stack build
	llvm-as-3.8 $(LIB_DIR)/*.ll
	ln -fs $(JLC_EXE) $(PROJECT_ROOT)
	# TODO We should let stack figure out where data-files are
	ln -fs $(LIB_DIR) $(JLC_DIR)

install:
	stack install

report:
	make -C doc

dist:   report
	stack sdist

.PHONY: test
test:   build
	@test -s test || { echo "Please see README.md"; exit 1; }
	make -C test test

checkdist: dist
	test/Grade $(TARBALL) -t test/testsuite

# Note, requires hlint post (fa0a76a412) as of this writing it haven't
# been merged into the official repo.
lint:
	hlint src --ignore-regex="src/Javalette/Syntax|src/Javalette/Parser/Main.hs"
