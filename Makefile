all: build

build:
	stack build
	llvm-as-3.8 lib/runtime.ll
	ln -fs `stack path --project-root`/`stack path --dist-dir`/build/jlc/jlc `stack path --project-root`

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
	test -s jlctests || { echo "Please see README.md"; exit 1; }
	make -C jlctests test
