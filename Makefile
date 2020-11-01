main:
	ocamlbuild -use-ocamlfind tuple.byte
	ocamlbuild -use-ocamlfind tupletest.byte
	ocamlbuild -use-ocamlfind operationtest.byte
	./tupletest.byte
	./operationtest.byte

clean:
	rm *.byte