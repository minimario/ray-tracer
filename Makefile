main:
	ocamlbuild -use-ocamlfind tuple.byte
	ocamlbuild -use-ocamlfind test/tupletest.byte
	./tupletest.byte

clean:
	rm *.byte
