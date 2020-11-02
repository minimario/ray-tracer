main:
	ocamlbuild -use-ocamlfind projectile.byte
	ocamlbuild -use-ocamlfind tuple.byte
	ocamlbuild -use-ocamlfind test/tupletest.byte
	./projectile.byte

clean:
	rm *.byte
