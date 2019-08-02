all:
	ocamlbuild -pkgs unix -use-menhir -menhir "menhir --explain --dump" main.native
	mv main.native mini-java
	cd tgc && $(MAKE)

clean:
	ocamlbuild -clean
	rm -f mini-java
