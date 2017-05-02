all: build

build:
	stack build
	llvm-as-3.8 lib/runtime.ll
	ln -fs `stack path --project-root`/`stack path --dist-dir`/build/jlc/jlc `stack path --project-root`
	# TODO We should let stack figure out where data-files are
	ln -fs `stack path --project-root`/lib/ `stack path --project-root`/`stack path --dist-dir`/build/jlc/

install:
	stack install

report: README.md
	pandoc README.md -o report.pdf \
	  --latex-engine=xelatex \
	  --variable urlcolor=cyan \
	  -V papersize:"a4paper"

dist:   report
	stack sdist

.PHONY: test
test:   build
	test -s test || { echo "Please see README.md"; exit 1; }
	make -C test test

PROJECT_ROOT = $(shell stack path --project-root)
DISTDIR = $(PROJECT_ROOT)/$(shell stack path --dist-dir)
TARBALL = $(DISTDIR)/$(shell stack list-dependencies | grep javalette | sed  's/ /-/').tar.gz

checkdist:
	stack sdist
	test/Grade $(TARBALL) -t test/testsuite
