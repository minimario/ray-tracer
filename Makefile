main:
	ocamlbuild -use-ocamlfind tuple.byte
	ocamlbuild -use-ocamlfind tupletest.byte
	./tupletest.byte

clean:
	rm *.byte