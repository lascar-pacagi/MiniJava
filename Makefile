all:
	ocamlbuild -use-menhir -menhir "menhir --explain --dump" main.native
	mv main.native mini-java

clean:
	ocamlbuild -clean
	rm -f mini-java
