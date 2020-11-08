TARGETS := projectile.byte tuple.byte color.byte canvas.byte
TEST_TARGETS := test/tupletest.byte test/colortest.byte test/canvastest.byte test/matrixtest.byte
main:
	ocamlbuild -use-ocamlfind $(TARGETS)
	./projectile.byte

runtest:
	ocamlbuild -use-ocamlfind $(TARGETS) $(TEST_TARGETS)
	./tupletest.byte
	./colortest.byte
	./canvastest.byte
	./matrixtest.byte

clean:
	rm *.byte
