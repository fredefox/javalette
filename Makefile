all: build

build:
	stack build

install:
	stack install

run:
	stack exec jlc

.PHONY: test
test:
	make -C jlctests test
