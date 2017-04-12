all: build

build:
	stack build
	ln -fs `stack path --project-root`/`stack path --dist-dir`/build/jlc `stack path --project-root`

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
