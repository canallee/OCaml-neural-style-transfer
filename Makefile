.PHONY: test check

build:
	dune build src
	dune build bin

utop:
	OCAMLRUNPARAM=b dune utop src

test:
	dune clean
	rm -f project.zip
	rm -rf data/output
	mkdir data/output
	rm -rf tmp
	rm -r data/output
	mkdir data/output
	touch data/output/cmd.png
	touch data/output/cmd0.png
	touch data/output/cmd1.png
	touch data/output/cmd.gif
	touch data/output/cmd0.gif
	touch data/output/cmd1.gif
	OCAMLRUNPARAM=b dune exec test/main.exe
	rm -r data/output
	mkdir data/output

launch:
	OCAMLRUNPARAM=b dune exec bin/main.exe

zip:
	rm -f project.zip
	zip -r project.zip . -x@exclude.lst

clean:
	dune clean
	rm -f project.zip
	rm -rf data/output
	mkdir data/output
	rm -rf tmp

docs:
	dune build @doc

loc:
	make clean
	cloc --by-file --include-lang=OCaml .
	make build
