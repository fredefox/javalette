all: build

build:
	stack build

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
test:
	test -s jlctests || { echo "Please see README.md"; exit 1; }
	make -C jlctests test
