## MiniJava Transpiler

`mini-java` is an educational transpiler from [MiniJava](http://www.cambridge.org/resources/052182060X/), which is a subset of [Java](https://en.wikipedia.org/wiki/Java_%28programming_language%29),
to [C](https://en.wikipedia.org/wiki/C_%28programming_language%29).\
The construction of this transpiler is documented on [www.mrcoder.org](https://www.mrcoder.org/en/compiler/minijava/).

### Install Dependencies

#### Linux (Ubuntu)

```bash
sudo apt-get install ocaml opam
opam init -a -y
eval `opam config env`
opam switch -y 4.07.0
eval `opam config env`
opam install -y ocamlbuild ocamlfind menhir
```

#### Mac OS X

```bash
brew install gpatch m4 ocaml opam
opam init -a -y
eval `opam config env`
opam switch -y 4.07.0
eval `opam config env`
opam install -y ocamlbuild ocamlfind menhir
```

### Download, Compile and Run

Once you have the dependencies (see above), run the following commands in your terminal.

```bash
git clone --recurse-submodules https://github.com/lascar-pacagi/MiniJava.git
make
./mini-java file.java
./file
```

---

The default C compiler is `cc`. if you want to use another compiler, you should give the name of the compiler as follow.

```bash
./mini-java --c-compiler clang file.java
```

---

If you want to launch `mini-java` from any directory, you should give the path to the `tgc` directory.

```bash
./mini-java --tgc-path "path to tgc directory" file.java
```

---

If you want to use `mini-java` without a garbage collector, and so without dependencies to `tgc`, you can
checkout the version 1.0. In this version, the transpiler doesn't use a garbage collector and only produces
the generated C file.

```bash
git checkout v1.0
make
./mini-java file.java
gcc file.c -o file
./file
```

If you want to modify the version 1.0, you can create a new branch from version 1.0 (for example `from_v1.0`).

```bash
git checkout -b from_v1.0 v1.0
```

to go back to the version with a garbage collector do the following.

```bash
git checkout master
make
```
