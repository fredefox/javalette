all: Grade

Grade: *.hs
	ghc -threaded --make Grade.hs
clean:
	rm -f *.{hi,o} Grade

distrib:
	rm -r tester ; true
	git clone . tester
	rm -rf tester/.git tester/.gitignore tester/report ; tester/*.py ; true
	tar -czf tester.tar.gz tester

test:   Grade
	./Grade ../.stack-work/install/x86_64-linux-nopie/lts-8.5/8.0.2/bin
