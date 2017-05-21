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
	test/Grade -g-no-pie -bLLVM $(TARBALL) \
		-t test/testsuite \
        -x arrays1 \
        -x arrays2


# Note, requires hlint post (fa0a76a412) as of this writing it haven't
# been merged into the official repo.
lint:
	hlint src --ignore-regex="src/Javalette/Syntax|src/Javalette/Parser/Main.hs"

bnfc: Javalette.cf
	# Calls bnfc, moves some stuff around and cleans up after itself
	bnfc Javalette.cf -p Javalette.Syntax -o src -m
	cd src ; \
		make ; \
		rm Javalette/Syntax/LexJavalette.x ; \
		rm Javalette/Syntax/ParJavalette.y ; \
		mv Javalette/Syntax/TestJavalette.hs Javalette/Parser/Main.hs ; \
		rm Javalette/Syntax/TestJavalette ; \
		mv Makefile.bak Makefile ; \
		sed -i '0,/Main/s//Javalette.Parser.Main/' Javalette/Parser/Main.hs
